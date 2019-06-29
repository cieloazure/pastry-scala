package pastry

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

object PastryNode {
  def props(myIpAddress: String, seedActor: ActorRef): Props = Props(new PastryNode(myIpAddress, Some(seedActor)))

  def props(myIpAddress: String): Props = Props(new PastryNode(myIpAddress))

  case object IdRequest
  case class JoinRequest(fromId: PastryNodeId)
  case class JoinResponseOK(fromId: PastryNodeId, trace: Array[Entry])
  case class JoinResponseNotOK(fromId: PastryNodeId)

  case class LeafSetRequest(fromId: PastryNodeId)
  case class LeafSetResponse(fromId: PastryNodeId, leafSet: Array[Entry])

  case class NeighbourhoodSetRequest(fromId: PastryNodeId)
  case class NeighbourhoodSetResponse(fromId: PastryNodeId, nbhdSet: Array[Entry])

  case class RoutingTableRequest(fromId: PastryNodeId, idx: Option[Int])
  case class RoutingTableResponse(fromId: PastryNodeId, routingTable: Array[Entry])
}

class PastryNode(val myIpAddress: String, val seedActor: Option[ActorRef] = None) extends Actor with ActorLogging {

  import PastryNode._

  val _id = new PastryNodeId(myIpAddress, 2)

  var _routingTable = new PastryRoutingTable(Entry(_id, self), PastryConstants.NODES, PastryConstants.TWO_RAISED_TO_BASE, (e1: Entry, e2: Entry) => e1.compareTo(e2))
  var _leafSet = new LeafSet[Entry](Entry(_id, self), PastryConstants.TWO_RAISED_TO_BASE, (e1: Entry, e2: Entry) => e1.compareTo(e2))
  var _neighbourhoodSet = new PastryNeighbourhoodSet(Entry(_id, self), PastryConstants.TWO_RAISED_TO_BASE)

  override def preStart(): Unit = {
    log.info(s"Starting node with id ${id.getHex}")
    seedActor.getOrElse({
      // Empty pastry network
      return
    })

    // At least one pastry node present
    seedActor.get ! JoinRequest(_id)
  }

  def deliver(msg: String, key: PastryNodeId, trace: Array[ActorRef]): Unit = {
    log.info(s"[Pastry Node ${id.getHex}]: Received ${msg} with ${key}")
  }

  def routingLogic(key: PastryNodeId): Option[Entry] = {
    // use routingTable, leafSet and neighbourhoodSet for routing and get appropriate node

    // Check leafSet
    val leafNode = _leafSet.getNode(Entry(key, null), (e1: Entry, e2: Entry) => e1.id.diff(e2.id))
    if (leafNode.isDefined) {
      return Some(leafNode.get)
    }

    // Check routingTable
    val routingNode = _routingTable.getNode(key)
    if (routingNode.isDefined) {
      return Some(routingNode.get)
    }

    // Rare case
    // Take union of all
    val combinedState: Array[Entry] = getUnion
    if (combinedState.isEmpty) {
      return None
    }

    val minCommonPrefix = key.findCommonPrefix(_id)

    //filter out combinedState nodes which have common prefix less than current node's id
    val filtered: Array[Entry] = combinedState.filter(_.id.findCommonPrefix(key) >= minCommonPrefix)
    if(filtered.isEmpty){
      return None
    }

    //if any nodes are remaining try to find a node whose difference is less than current node
    val minIdx: Int = filtered.map(_.id.diff(key)).zipWithIndex.min._2

    Some(filtered(minIdx))
  }

  def id: PastryNodeId = _id

  def getUnion: Array[Entry] = {
    (_routingTable.getTable ++ _leafSet.getSet ++ _neighbourhoodSet.getSet).distinct
  }

  def updateState(nodes: Array[Entry]): Unit = {
    this._leafSet.updateSet(nodes)
    this._routingTable.update(nodes)
  }

  def updateNeighbourhood(nodes: Array[Entry]): Unit = {
    this._neighbourhoodSet.update(nodes)
    updateState(nodes)
  }

  override def receive: Receive = {
    case IdRequest =>
      sender() ! id.getHex

    case JoinRequest(from) =>
      val node: Option[Entry] = routingLogic(from)
      if(node.isEmpty) {
        sender() ! JoinResponseNotOK(id)
      } else {
        // Initialize trace
        val trace:Array[Entry] = Array[Entry](node.get)
        if(node.get.id == this.id) {
          sender() ! JoinResponseOK(id, trace)
        } else {
          //node ! Route("join", id, trace, sender())
          sender() ! "routing...(not implemented)"
        }
      }

    case JoinResponseOK(from, trace) =>
      log.info(s"[${id.getHex}]Received OK response from ${from.getHex}")

      // A suitable node is found which is numerically close
      // can populate leaf set from last node
      trace.last.actor ! LeafSetRequest

      // The first node in the trace is close by proximity hence
      // neighbourhood set can be populated
      trace.head.actor ! NeighbourhoodSetRequest

//      if(trace.length == 1){
//        // Get the entire routing table and populate routing table
//        trace.head.actor ! RoutingTableRequest(None)
//      } else {
//        // If intermediate nodes present, query these nodes to get
//        // routing table entries
//        val middleTrace: Array[Entry] = trace.drop(1).dropRight(1)
//        for((node, idx) <- middleTrace.zipWithIndex){
//          node.actor ! RoutingTableRequest(Some(idx))
//        }
//      }


    case JoinResponseNotOK(from) =>
      log.info(s"[${id.getHex}]Received Not OK response from ${from.getHex}")

      // No suitable node found which is numerically close
      // This means that network is sparse
      // Request all sets and update
      // But...
      // As the node is close by proximity metric
      // can just populate neighbourhood set
      sender() ! NeighbourhoodSetRequest(id)
      sender() ! LeafSetRequest(id)
      sender() ! RoutingTableRequest(id, None)
//      sendJoinMessage()

     case LeafSetRequest(from) =>
       sender() ! LeafSetResponse(id, _leafSet.getSet)

     case LeafSetResponse(from, hisLeafSet) =>
       updateState(hisLeafSet)

     case NeighbourhoodSetRequest(from) =>
       sender() ! NeighbourhoodSetResponse(id, _neighbourhoodSet.getSet)

     case NeighbourhoodSetResponse(from, hisNeighbourhoodSet) =>
       updateNeighbourhood(hisNeighbourhoodSet)

      case RoutingTableRequest(from, id) =>
        sender() ! RoutingTableResponse(from, this._routingTable.getTable)

      case RoutingTableResponse(from, hisRoutingTable) =>
       updateState(hisRoutingTable)

    // case Route("join", id, trace, replyTo) =>
    //      node = routingLogic()
    // newTrace = trace +: self
    //      if(node == self) sender() ! JoinResponse(trace)
    //     node ! Route("join", id, trace, replyTo)

    //    case Route(msg, key, traceRoute, replyTo) =>
    // routingLogic()

//     case NewNodeJoined(who) =>
//      updateState(who, sender())

      case msg: String =>
        log.info(s"[TEST MESSAGE]: ${msg}")
  }

  // TODO
  // def forward()
  // def route()
  // def distance(other: Actor[PastryNode])

}
