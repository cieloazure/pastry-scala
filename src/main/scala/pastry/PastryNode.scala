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

  def routingLogic(key: String): Option[Entry2] = {
    // use routingTable, leafSet and neighbourhoodSet for routing and get appropriate node
    val keyEntry = Entry2(key, null, null)

    val diffFn = (e1: Entry2, e2: Entry2) => {
      (e1.toInt(PastryConstants.BASE) - e2.toInt(PastryConstants.BASE)).abs
    }

    // Check leafSet
    val leafNode = _leafSet.getNode(keyEntry, diffFn)
    if (leafNode.isDefined) {
      return Some(leafNode.get)
    }

    // Check routingTable
    val routingNode = _routingTable.getNode(keyEntry)
    if (routingNode.isDefined) {
      return Some(routingNode.get)
    }

    // Rare case
    // Take union of all
    val combinedState: Array[Entry2] = getUnion
    if (combinedState.isEmpty) {
      return None
    }

    val minCommonPrefix = _prefixFn(keyEntry, _hostEntry)

    //filter out combinedState nodes which have common prefix less than current node's id
    val filtered: Array[Entry2] = combinedState.filter(_prefixFn(keyEntry, _) >= minCommonPrefix)
    if(filtered.isEmpty){
      return None
    }

    //if any nodes are remaining try to find a node whose difference is less than current node
    val minIdx: Int = filtered.map(diffFn(_, keyEntry)).zipWithIndex.min._2

    Some(filtered(minIdx))
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
    addNodeToNeighbourhoodSet(node)
    addNodeToRoutingTable(node)
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
      node._actor ! JoinComplete(_hostEntry)
    }
  }

  override def preStart(): Unit = {
    log.info(s"Starting node with id $id at location ${_location.x},${_location.y}")
    val seedEntryActor = seedEntry.getOrElse({
      // Empty pastry network
      return
    })._actor

    // At least one pastry node present
    seedEntryActor ! JoinRequest(_hostEntry)
  }


  override def receive: Receive = {
    case IdRequest =>
      sender() ! id

    case JoinRequest(from) =>
      // Initialize trace
      val trace:Array[Entry2] = Array[Entry2](_hostEntry)
      log.info(s"[${id}]Received [JoinRequest] response from ${from}")
      val node: Option[Entry2] = routingLogic(from._id)
      log.info(s"[${id}] Routing Logic returned ${node}")
      if(node.isEmpty) {
        sender() ! JoinResponseNotOK(_hostEntry)
      } else {
        if(trace.contains(node)) {
          sender() ! JoinResponseOK(_hostEntry, trace)
        } else {
          //node ! Route("join", id, trace, sender())
          node.get._actor ! RouteRequest(from, JOIN, trace)
        }
      }
      println(_leafSet.getSet.length + " + " +_routingTable.getTable.length + " + " +_neighbourhoodSet.getSet.length)

    case JoinResponseOK(from, trace) =>
      log.info(s"[${id}]Received [JoinResponseOK] response from ${from}")

      // A suitable node is found which is numerically close
      // can populate leaf set from last node
      trace.last._actor ! LeafSetRequest(_hostEntry, JOIN)

      // The first node in the trace is close by proximity hence
      // neighbourhood set can be populated
      trace.head._actor ! NeighbourhoodSetRequest(_hostEntry, JOIN)

      if(trace.length == 1){
        // Get the entire routing table and populate routing table
        trace.head._actor ! RoutingTableRequest(_hostEntry, None, JOIN)
      } else {
        // If intermediate nodes present, query these nodes to get
        // routing table entries
        val middleTrace: Array[Entry2] = trace.slice(1, trace.length - 1)
        for((node, idx) <- middleTrace.zipWithIndex){
          node._actor ! RoutingTableRequest(_hostEntry, Some(idx), JOIN)
        }
      }
//      sendJoinCompleteMessage()


    case JoinResponseNotOK(from) =>
      log.info(s"[${id}]Received [JoinResponseNotOK] response from ${from}")

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
      println(_leafSet.getSet.length + " + " +_routingTable.getTable.length + " + " +_neighbourhoodSet.getSet.length)
//      sendJoinCompleteMessage()

    case JoinComplete(from) =>
      log.info(s"[${id}] Received [JoinComplete] response from ${from}")
      addNodeToState(from)
      println(_leafSet.getSet.length + " + " +_routingTable.getTable.length + " + " +_neighbourhoodSet.getSet.length)

    case LeafSetRequest(from, purpose) =>
      log.info(s"[${id}] Received [LeafSetRequest] from ${from}")
      println(_leafSet.getSet.length + " + " +_routingTable.getTable.length + " + " +_neighbourhoodSet.getSet.length)
      sender() ! LeafSetResponse(_hostEntry, _leafSet.getSet, purpose)

    case LeafSetResponse(from, hisLeafSet, purpose) =>
      log.info(s"[${id}] Received [LeafSetResponse] from ${from} with ${hisLeafSet.length}")
      updateState(hisLeafSet)
      purpose match {
        case JOIN => sendJoinCompleteMessage(_leafSet.getSet)
      }

    case NeighbourhoodSetRequest(from, purpose) =>
      log.info(s"[${id}] Received [NeighbourhoodSetRequest] from ${from}")
      println(_leafSet.getSet.length + " + " +_routingTable.getTable.length + " + " +_neighbourhoodSet.getSet.length)
      sender() ! NeighbourhoodSetResponse(_hostEntry, _neighbourhoodSet.getSet, purpose)

    case NeighbourhoodSetResponse(from, hisNeighbourhoodSet, purpose) =>
      log.info(s"[${id}] Received [NeighbourhoodSetResponse] from ${from} with ${hisNeighbourhoodSet.length}")
      updateNeighbourhood(hisNeighbourhoodSet)
      purpose match {
        case JOIN => sendJoinCompleteMessage(_neighbourhoodSet.getSet)
      }

    case RoutingTableRequest(from, idx, purpose) =>
      log.info(s"[${id}] Received [RoutingTableRequest] from ${from}")
      println(_leafSet.getSet.length + " + " +_routingTable.getTable.length + " + " +_neighbourhoodSet.getSet.length)
      if(idx.isEmpty)
        sender() ! RoutingTableResponse(from, idx, this._routingTable.getTable, purpose)
      else
        sender() ! RoutingTableResponse(from, idx, this._routingTable.getTableRow(idx.get), purpose)

    case RoutingTableResponse(from, idx, hisRoutingTable, purpose) =>
      log.info(s"[${id}] Received [RoutingTableResponse] from ${from} with ${hisRoutingTable.length}")
      updateState(hisRoutingTable, idx)
      purpose match {
        case JOIN =>
          if(idx.isEmpty){
            sendJoinCompleteMessage(_routingTable.getTable)
          } else {
            sendJoinCompleteMessage(_routingTable.getTableRow(idx.get))
          }
      }

    case RouteRequest(from, JOIN, trace) =>
      val newTrace = trace :+ _hostEntry
      log.info(s"[$id] Received [RouteRequest] with [${JOIN}]")
      val node: Option[Entry2] = routingLogic(from._id)
      log.info(s"[${id}] Routing Logic returned ${node}")
      log.info(s"${trace.contains(node.get)}")
      if(node.isEmpty) {
        from._actor ! JoinResponseOK(_hostEntry, trace)
      } else {
        if(trace.contains(node.get)) {
          // Terminates at this node
          // Or sending it back to a previous node
          // Then terminate
          from._actor ! JoinResponseOK(_hostEntry, trace)
        } else {
          // Forward to other node
          node.get._actor ! RouteRequest(from, JOIN, newTrace)
        }
      }

    case RouteRequest(from, msg, trace) =>
      log.info("routing a message other than join")

    case msg: String =>
      log.info(s"[TEST MESSAGE]: ${msg}")
  }

  // TODO
  // def forward()
  // def route()
  // def distance(other: Actor[PastryNode])

}
