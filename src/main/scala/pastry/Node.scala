package pastry

import akka.actor.{Actor, ActorRef, Props}

object Node {
  def props(myIpAddress: String, myId: String, myLocation: Location, statActor: ActorRef): Props
  = Props(new Node(myIpAddress, None, Some(myId), Some(myLocation), statsActor = Some(statActor)))

  def props(myIpAddress: String, seedEntry: Entry, myId: String, myLocation: Location, statActor: ActorRef): Props
  = Props(new Node(myIpAddress, Some(seedEntry), Some(myId), Some(myLocation), statsActor = Some(statActor)))

  case class JoinRequest(fromNode: Entry)
  case class JoinRequestOK(trace: Array[Entry])
  case object JoinRequestFailed
  case object LeafSetRequest
  case class LeafSetResponse(leafSet: Array[Entry])
  case object RoutingTableRequest
  case class RoutingTableResponse(routingTable: Array[Entry])
  case class RoutingTableRowRequest(idx: Int)
  case class RoutingTableRowResponse(idx: Int, routingTable: Array[Entry])
  case class NeighbourhoodSetRequest(from: Entry)
  case class NeighbourhoodSetResponse(from: Entry, neighbourhoodSet: Array[Entry])
  case class HandleJoinRequest(host: Entry, forNode: Entry, respondTo: ActorRef)
  case class HandleForwardJoinRequest(host: Entry, forNode: Entry, trace: Array[Entry], respondTo: ActorRef)
  case class ForwardJoinRequest(forNode: Entry, trace: Array[Entry], respondTo: ActorRef)
  case object ForwardJoinRequestAck
  case class UpdateState(state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]))
  case object StartFixer
  case object Ping
  case object Pong
  case class RequestOK(newState: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]))
  case object RequestFailed
  case class NewNodeJoined(whichNode: Entry)
  case object StateFixed
  case class JoinStats(hops: Int)
  case object AvgJoinHopsRequest
  case class RouteRequest(key: String)
  case class HandleRouteRequest(host: Entry, key: String, respondTo: ActorRef)
  case class ForwardRouteRequest(key: String, trace: Array[Entry], respondTo: ActorRef)
  case object ForwardRouteRequestAck
  case class HandleForwardRouteRequest(host: Entry, key: String, trace: Array[Entry], respondTo: ActorRef)
  case class RouteRequestOK(trace: Array[Entry])
}

class Node (val myIpAddress: String,
            val seedEntry: Option[Entry] = None,
            val myId: Option[String] = None,
            val myLocation: Option[Location] = None,
            val statsActor: Option[ActorRef] = None,
           ) extends Actor with StateUpdatable {

  import Node._

  /**
    * VARIABLES
    */
  val _id: String =  myId.get //new PastryNodeId(myIpAddress)
  val _location: Location = myLocation.get
  val _statActor: ActorRef = statsActor.get
  val _hostNode = Entry(_id, self, _location)
  val _state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]) = constructState(_hostNode, _location)

  /**
    * Step 1: Determine seed or normal
    * PreStart: Before the actor begins
    */
  override def preStart(): Unit = {
    println(s"[${_id}] Starting node with id ${_id} at location ${_location.x},${_location.y}")
    if(seedEntry.isDefined) {
      println(s"[${_id}] Sending join request to seed node ${seedEntry.get._id}")
      context.become(waitForJoinResponse(_state))
      context.actorOf(RequestActor.props(1, seedEntry.get._actor, JoinRequest(_hostNode), _state),
        "request-actor-join-context")
    } else {
      val router = context.actorOf(Router.props(_state), "router")
      context.become(active(_state, router))
    }
  }


  /**
    * Step 2: Wait for joining
    * @param state
    * @return
    */
  def waitForJoinResponse(state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])): Receive = {
    case JoinRequestOK(trace) =>
      println(s"[${_id}] Join request OK received with trace length ${trace.length}. Building state...")
      val waitFor = if(trace.length > 2) trace.length else  2
      _statActor ! JoinStats(trace.length)
      context.become(stateBuilder(waitFor, state))
      context.actorOf(RequestActor.props(3, trace.head._actor, NeighbourhoodSetRequest(_hostNode), state), "request-actor-nbhd-set-request")
      context.actorOf(RequestActor.props(2, trace.last._actor, LeafSetRequest, state), "request-actor-leaf-set-request")
      if(trace.length > 2) {
        context.actorOf(
          RouteRequestActor.props(trace.length - 2, trace.slice(1, trace.length - 1).map(_._actor), _hostNode, state), "request-actor-routing-table-row-request")
      }

    case JoinRequestFailed =>
      println("TODO: Choose a different seed actor and start the join process again")
  }


  /**
    * Step 3: Build State
    *
    * @param requestsLeft
    * @param state
    * @return
    */
  def stateBuilder(requestsLeft: Int,
                          state:(LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])): Receive = {
    case RequestOK(newState) =>
      val mergedState = mergeState(state, newState)
      val requestsLeftNow = requestsLeft - 1
      if(requestsLeftNow == 0) {
        println(s"[${_id}] State built: ${getStateString(mergedState)}, becoming active..")
        val routerRef = context.actorOf(Router.props(mergedState), "router")
        sendNewNodeJoinedMessages(mergedState)
        context.become(active(mergedState, routerRef))
      } else {
        context.become(stateBuilder(requestsLeftNow, mergedState))
      }

    case RequestFailed =>
      println("TODO: Choose a different seed actor and start the join process again")
  }


  /**
    * Step 4: Active now
    * @param state
    * @param router
    * @return
    */
  def active(state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]), router: ActorRef): Receive = {
    case JoinRequest(fromNode) =>
      println(s"[${_id}] Join request receive from ${fromNode._id}")
      router ! HandleJoinRequest(_hostNode, fromNode, sender())

    case ForwardJoinRequest(forNode, trace, respondTo) =>
      sender() ! ForwardJoinRequestAck
      router ! HandleForwardJoinRequest(_hostNode, forNode, trace, respondTo)

    case LeafSetRequest =>
      sender() ! LeafSetResponse(getLeafSet(state))

    case RoutingTableRequest =>
      sender() ! RoutingTableResponse(getRoutingTable(state))

    case RoutingTableRowRequest(idx) =>
      sender() ! RoutingTableRowResponse(idx, getRoutingTableRow(state, idx))

    case NeighbourhoodSetRequest(from) =>
      sender() ! NeighbourhoodSetResponse(_hostNode, getNeighbourhoodSet(state))
      val newState = updateNeighbourhood(state, makeArray(from))
      router !  UpdateState(newState)
      context.become(active(newState, router))

    case UpdateState(newState) =>
      router ! UpdateState(newState)
      context.become(active(newState, router))

    case NewNodeJoined(whichNode) =>
      val newState = updateState(state, makeArray(whichNode))
      println(s"[${_id}] New node joined the network. State now: ${getStateString(newState)}")
      context.become(active(newState, router))
      router ! UpdateState(newState)

    case RouteRequest(key) =>
      println(s"[${_id}] Route request receive for key ${key}")
      router ! HandleRouteRequest(_hostNode, key, sender())

    case ForwardRouteRequest(key, trace, respondTo) =>
      sender() ! ForwardRouteRequestAck
      router ! HandleForwardRouteRequest(_hostNode, key, trace, respondTo)

    case Ping =>
      sender() ! Pong
  }


  /**
    *
    * PRIVATE FUNCTIONS
    *
    */

  private def getLeafSet(state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])):
  Array[Entry] = state._1.getAllElem
  private def getRoutingTable(state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])):
  Array[Entry] = state._2.getAllElem
  private def getRoutingTableRow(state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]), idx: Int):
  Array[Entry] = state._2.getAllElemFromRow(idx)
  private def getNeighbourhoodSet(state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])) :
  Array[Entry] = state._3.getAllElem
  private def constructState(host: Entry,
                             location: Location):
  (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]) = {
    import PastryConstants._
    val _numEntries = BASE
    val _numRows = NODES

    val _distanceComparator: (Entry, Entry) => Boolean = (e1: Entry, e2: Entry) => {
      e1._location.distance(this._location).compareTo(e2._location.distance(this._location)) < 0
    }

    val _numericComparator: (Entry, Entry) => Boolean = (e1: Entry, e2: Entry) => {
      e1.toInt(PastryConstants.BASE).compareTo(e2.toInt(PastryConstants.BASE)) < 0
    }

    val _diffFn = (e1: Entry, e2: Entry) => {
      (e1.toInt(PastryConstants.BASE) - e2.toInt(PastryConstants.BASE)).abs
    }

    val _prefixFn = (e1: Entry, e2: Entry) => {
      e1.findCommonPrefix(e2)
    }

    val _leafSet = new LeafSet[Entry](_hostNode, _numEntries, _distanceComparator, _numericComparator, _diffFn)
    val _routingTable = new RoutingTable[Entry](_hostNode, _numRows, _numEntries, _distanceComparator, _prefixFn)
    val _neighbourhoodSet = new NeighbourhoodSet[Entry](_hostNode, _numEntries, _distanceComparator)

    (
      _leafSet,
      _routingTable,
      _neighbourhoodSet
    )
  }
  private def sendNewNodeJoinedMessages(newState: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])) :
  Unit =  {
    for(node <- getUnion(newState)) {
      if(node._id != _hostNode._id) node._actor ! NewNodeJoined(_hostNode)
    }
  }
  private def getUnion(state:(LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])):
  Array[Entry] = (state._1.getAllElem ++ state._2.getAllElem ++ state._3.getAllElem).distinct
  private def makeArray(elem: Entry): Array[Entry] = Array[Entry](elem)
  private def mergeState(state1: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]),
                         state2: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])):
  (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]) = {
    val newLeafSet = state1._1.update(state2._1.getAllElem)
    val newRoutingTable = state1._2.update(state2._2.getAllElem)
    val newNbhdSet = state1._3.update(state2._3.getAllElem)
    (newLeafSet, newRoutingTable, newNbhdSet)
  }
  private def makeStr(entries: Array[Entry]): String = {
    entries.map(_._id).mkString(",")
  }
  private def printState(state:(LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])): Unit = {
    println(getStateString(state))
  }
  private def getStateString(state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])): String = {
    s"[${_id}] State: ([${makeStr(state._1.getAllElem)}] + [${makeStr(state._2.getAllElem)}] " +
      s"+ [${makeStr(state._3.getAllElem)}])"
  }
  private def printArray(message: String, entries: Array[Entry]): Unit = {
    println(s"[${_id}] ${message}: [${makeStr(entries)}]")
  }
  override def receive: Receive = Actor.emptyBehavior
}
