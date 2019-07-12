package pastry

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

object PastryNode {
  def props(myIpAddress: String, seedEntry: Entry2): Props = Props(new PastryNode(myIpAddress, Some(seedEntry)))
  def props(myIpAddress: String): Props = Props(new PastryNode(myIpAddress))
  def props(myIpAddress: String, myId: String, myLocation: Location): Props
  = Props(new PastryNode(myIpAddress, None, Some(myId), Some(myLocation)))
  def props(myIpAddress: String, seedEntry: Entry2, myId: String, myLocation: Location): Props
  = Props(new PastryNode(myIpAddress, Some(seedEntry), Some(myId), Some(myLocation)))

  case object IdRequest
  case class IdResponse(id: String)

  case class JoinRequest(from: Entry2)
  case class JoinResponseOK(from: Entry2, trace: Array[Entry2])
  case class JoinResponseNotOK(from: Entry2)
  case class JoinComplete(fromId: Entry2)

  case class LeafSetRequest(from: Entry2, purpose: String)
  case class LeafSetResponse(from: Entry2, leafSet: Array[Entry2], purpose: String)

  case class NeighbourhoodSetRequest(from: Entry2, purpose: String)
  case class NeighbourhoodSetResponse(from: Entry2, nbhdSet: Array[Entry2], purpose: String)

  case class RoutingTableRequest(from: Entry2, idx: Option[Int], purpose: String)
  case class RoutingTableResponse(from: Entry2, idx: Option[Int], routingTable: Array[Entry2], purpose: String)

  case class RouteRequest(from: Entry2, msg: String, trace: Array[Entry2])
}

class PastryNode(val myIpAddress: String,
                 val seedEntry: Option[Entry2] = None,
                 val myId: Option[String] = None,
                 val myLocation: Option[Location] = None
                ) extends Actor
  with ActorLogging {

  import PastryNode._

  val _id: String =  myId.get //new PastryNodeId(myIpAddress)
  val _location: Location = myLocation.get

  private val _hostEntry = Entry2(_id, self, _location)
  private val _numEntries = PastryConstants.BASE
  private val _numRows = PastryConstants.NODES

  val _distanceComparator: (Entry2, Entry2) => Int = (e1: Entry2, e2: Entry2) => {
    e1._location.distance(this._location).compareTo(e2._location.distance(this._location))
  }

  val _numericComparator: (Entry2, Entry2) => Int = (e1: Entry2, e2: Entry2) => {
    e1.toInt(PastryConstants.BASE).compareTo(e2.toInt(PastryConstants.BASE))
  }

  private val _prefixFn = (e1: Entry2, e2: Entry2) => {
    e1.findCommonPrefix(e2)
  }

  val _leafSet = new LeafSet[Entry2](_hostEntry, _numEntries, _distanceComparator, _numericComparator)
  val _routingTable = new RoutingTable[Entry2](_hostEntry, _numRows, _numEntries, _distanceComparator, _prefixFn)
  val _neighbourhoodSet = new NeighbourhoodSet[Entry2](_hostEntry, _numEntries, _distanceComparator)

  val JOIN = "join"


  def routingLogic(key: String): Option[Tuple2[Entry2, String]] = {
    // use routingTable, leafSet and neighbourhoodSet for routing and get appropriate node
    val keyEntry = Entry2(key, null, null)

    val diffFn = (e1: Entry2, e2: Entry2) => {
      (e1.toInt(PastryConstants.BASE) - e2.toInt(PastryConstants.BASE)).abs
    }

    // Check leafSet
    val leafNode = _leafSet.getNode(keyEntry, diffFn)
    if (leafNode.isDefined) {
      return Some(leafNode.get, "FROM_LEAF_SET")
    }

    // Check routingTable
    val routingNode = _routingTable.getNode(keyEntry)
    if (routingNode.isDefined) {
      return Some(routingNode.get, "FROM_ROUTING_TABLE")
    }

    // Rare case
    // Take union of all
    val combinedState: Array[Entry2] = getUnion
    if (combinedState.isEmpty) {
      return None
    }

    val minCommonPrefix = _prefixFn(keyEntry, _hostEntry)

    //filter out combinedState nodes which have common prefix less than current node's id
    val filtered: Array[Entry2] = combinedState.filter(_prefixFn(keyEntry, _) >= minCommonPrefix).filter(_._id != id)
    if(filtered.isEmpty){
      return None
    }

    //if any nodes are remaining try to find a node whose difference is less than current node
    val minIdx: Int = filtered.map(diffFn(_, keyEntry)).zipWithIndex.min._2

    Some(filtered(minIdx), "FROM_UNION")
  }

  def id: String = _id

  def getUnion: Array[Entry2] = {
    (_routingTable.getTable ++ _leafSet.getSet ++ _neighbourhoodSet.getSet).distinct
  }

  def updateState(nodes: Array[Entry2], idx: Option[Int] = None): Unit = {
    this._leafSet.updateSet(nodes)
    if(idx.isEmpty)
      this._routingTable.updateTable(nodes)
    else
      this._routingTable.updateTableRow(nodes, idx.get)
  }

  def updateNeighbourhood(nodes: Array[Entry2]): Unit = {
    this._neighbourhoodSet.updateSet(nodes)
    updateState(nodes)
  }

  def addNodeToState(node: Entry2): Unit = {
    addNodeToLeafSet(node)
    addNodeToRoutingTable(node)
    addNodeToNeighbourhoodSet(node)
  }

  def addNodeToLeafSet(node: Entry2): Unit = {
    _leafSet.updateSet(Array[Entry2](node))
  }

  def addNodeToNeighbourhoodSet(node: Entry2): Unit = {
    _neighbourhoodSet.updateSet(Array[Entry2](node))
  }

  def addNodeToRoutingTable(node: Entry2): Unit = {
    _routingTable.updateTable(Array[Entry2](node))
  }


  def sendJoinCompleteMessage(nodes: Array[Entry2]): Unit = {
    for(node <- nodes){
      if(node._id != id) node._actor ! JoinComplete(_hostEntry)
    }
  }

  override def preStart(): Unit = {
    println(s"Starting node with id $id at location ${_location.x},${_location.y}")
    val seedEntryActor = seedEntry.getOrElse({
      // Empty pastry network
      return
    })._actor

    // TODO: Maybe spawn a join actor

    // At least one pastry node present
    context.become(joinResponseContext)
    updateNeighbourhood(Array[Entry2](seedEntry.get))
    seedEntryActor ! JoinRequest(_hostEntry)
  }

  def waitingForStateResponsesContext(waitingFor: Int): Receive = {
    case LeafSetResponse(from, hisLeafSet, purpose) =>
      println(s"[${id}] Received [LeafSetResponse] from ${from} with ${hisLeafSet.length}")
      updateState(hisLeafSet)
      val remaining = waitingFor - 1
      if(remaining > 0) {
        context.become(waitingForStateResponsesContext(remaining))
      } else {
        println(s"[$id] Current State:" + _leafSet.getSet.length + "+" + _routingTable.getTable.length + "+" + _neighbourhoodSet.getSet.length)
        sendJoinCompleteMessage(getUnion)
        context.become(activeContext)
      }

    case NeighbourhoodSetResponse(from, hisNeighbourhoodSet, purpose) =>
      println(s"[${id}] Received [NeighbourhoodSetResponse] from ${from} with ${hisNeighbourhoodSet.length}")
      updateNeighbourhood(hisNeighbourhoodSet)
      val remaining = waitingFor - 1
      if(remaining > 0){
        context.become(waitingForStateResponsesContext(remaining))
      }else{
        println(s"[$id] Current State:" + _leafSet.getSet.length + "+" + _routingTable.getTable.length + "+" + _neighbourhoodSet.getSet.length)
        sendJoinCompleteMessage(getUnion)
        context.become(activeContext)
      }

    case RoutingTableResponse(from, idx, hisRoutingTable, purpose) =>
      println(s"[${id}] Received [RoutingTableResponse] from ${from} with ${hisRoutingTable.length}")
      updateState(hisRoutingTable, idx)
      val remaining = waitingFor - 1
      if(remaining > 0){
        context.become(waitingForStateResponsesContext(remaining))
      }else{
        println(s"[$id] Current State:" + _leafSet.getSet.length + "+" + _routingTable.getTable.length + "+" + _neighbourhoodSet.getSet.length)
        sendJoinCompleteMessage(getUnion)
        context.become(activeContext)
      }
  }

  def joinResponseContext: Receive = {
    case JoinResponseOK(from, trace) =>
      println(s"[${id}] Received [JoinResponseOK] response from ${from}")

      var counter = 0
      // A suitable node is found which is numerically close
      // can populate leaf set from last node
      trace.last._actor ! LeafSetRequest(_hostEntry, JOIN)
      counter += 1

      // The first node in the trace is close by proximity hence
      // neighbourhood set can be populated
      trace.head._actor ! NeighbourhoodSetRequest(_hostEntry, JOIN)
      counter += 1

      if(trace.length == 1){
        // Get the entire routing table and populate routing table
        trace.head._actor ! RoutingTableRequest(_hostEntry, None, JOIN)
        counter += 1
      } else {
        // If intermediate nodes present, query these nodes to get
        // routing table entries
        val middleTrace: Array[Entry2] = trace.slice(1, trace.length - 1)
        for((node, idx) <- middleTrace.zipWithIndex){
          node._actor ! RoutingTableRequest(_hostEntry, Some(idx), JOIN)
          counter += 1
        }
      }
      context.become(waitingForStateResponsesContext(counter))


    case JoinResponseNotOK(from) =>
      println(s"[${id}] Received [JoinResponseNotOK] response from ${from}")

      // No suitable node found which is numerically close
      // This means that network is sparse
      // Request all sets and update
      // But...
      // As the node is close by proximity metric
      // can just populate neighbourhood set
      sender() ! NeighbourhoodSetRequest(_hostEntry, JOIN)
      sender() ! LeafSetRequest(_hostEntry, JOIN)
      sender() ! RoutingTableRequest(_hostEntry, None, JOIN)
      addNodeToState(from)
      context.become(waitingForStateResponsesContext(3))
  }



  def activeContext: Receive = {
    case IdRequest =>
      sender() ! id

    case JoinRequest(from) =>
      // Initialize trace
      val trace:Array[Entry2] = Array[Entry2](_hostEntry)
      println(s"[${id}] Received [JoinRequest] response from ${from}")
      val node: Option[(Entry2, String)] = routingLogic(from._id)
      println(s"[${id}] Routing Logic returned ${node}")
      if(node.isEmpty) {
        sender() ! JoinResponseNotOK(_hostEntry)
      } else {
        if(trace.contains(node.get._1)) {
          sender() ! JoinResponseOK(_hostEntry, trace)
        } else {
          //node ! Route("join", id, trace, sender())
          node.get._1._actor ! RouteRequest(from, JOIN, trace)
        }
      }

    case JoinComplete(from) =>
      println(s"[${id}] Received [JoinComplete] response from ${from}")
      addNodeToState(from)
      println(s"[${id}] Current State:" + _leafSet.getSet.length + "+" + _routingTable.getTable.length + "+" + _neighbourhoodSet.getSet.length)

    case LeafSetRequest(from, purpose) =>
      println(s"[${id}] Received [LeafSetRequest] from ${from}")
      sender() ! LeafSetResponse(_hostEntry, _leafSet.getSet, purpose)

    case LeafSetResponse(from, hisLeafSet, purpose) =>
      println(s"[${id}] Received [LeafSetResponse] from ${from} with ${hisLeafSet.length}")
      updateState(hisLeafSet)

    case NeighbourhoodSetRequest(from, purpose) =>
      println(s"[${id}] Received [NeighbourhoodSetRequest] from ${from}")
      sender() ! NeighbourhoodSetResponse(_hostEntry, _neighbourhoodSet.getSet, purpose)

    case NeighbourhoodSetResponse(from, hisNeighbourhoodSet, purpose) =>
      println(s"[${id}] Received [NeighbourhoodSetResponse] from ${from} with ${hisNeighbourhoodSet.length} entries")
      updateNeighbourhood(hisNeighbourhoodSet)

    case RoutingTableRequest(from, idx, purpose) =>
      println(s"[${id}] Received [RoutingTableRequest] from ${from}")
      if(idx.isEmpty)
        sender() ! RoutingTableResponse(from, idx, this._routingTable.getTable, purpose)
      else
        sender() ! RoutingTableResponse(from, idx, this._routingTable.getTableRow(idx.get), purpose)

    case RoutingTableResponse(from, idx, hisRoutingTable, purpose) =>
      println(s"[${id}] Received [RoutingTableResponse] from ${from} with ${hisRoutingTable.length} entries")
      updateState(hisRoutingTable, idx)

    case RouteRequest(from, JOIN, trace) =>
      val newTrace = trace :+ _hostEntry
      println(s"[$id] Received [RouteRequest] with [${JOIN}]")
      val node: Option[(Entry2, String)] = routingLogic(from._id)
      println(s"[${id}] Routing Logic returned ${node}")
      if(node.isEmpty) {
        from._actor ! JoinResponseOK(_hostEntry, trace)
      } else {
        if(newTrace.contains(node.get._1)) {
          // Terminates at this node
          // Or sending it back to a previous node
          // Then terminate
          from._actor ! JoinResponseOK(_hostEntry, newTrace)
        } else {
          // Forward to other node
          node.get._1._actor ! RouteRequest(from, JOIN, newTrace)
        }
      }

    case RouteRequest(from, msg, trace) =>
      println("routing a message other than join")

    case msg: String =>
      println(s"[TEST MESSAGE]: ${msg}")
  }

  override def receive: Receive = activeContext

  // TODO
  // def forward()
  // def route()
  // def distance(other: Actor[PastryNode])

}
