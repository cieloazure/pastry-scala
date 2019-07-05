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

  case class LeafSetRequest(from: Entry2)
  case class LeafSetResponse(from: Entry2, leafSet: Array[Entry2])

  case class NeighbourhoodSetRequest(from: Entry2)
  case class NeighbourhoodSetResponse(from: Entry2, nbhdSet: Array[Entry2])

  case class RoutingTableRequest(from: Entry2, idx: Option[Int])
  case class RoutingTableResponse(from: Entry2, idx: Option[Int], routingTable: Array[Entry2])
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

  def routingLogic(key: String): Option[Entry2] = {
    // use routingTable, leafSet and neighbourhoodSet for routing and get appropriate node
    val keyEntry = Entry2(key, null, null)

    // Check leafSet
    val diffFn = (e1: Entry2, e2: Entry2) => {
      (e1.toInt(PastryConstants.BASE) - e2.toInt(PastryConstants.BASE)).abs
    }

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


  def sendJoinMessage(): Unit = {
    val allNodes = getUnion
    for(node <- allNodes){
      node._actor ! JoinComplete(_hostEntry)
    }
  }

  override def preStart(): Unit = {
    log.info(s"Starting node with id $id at location ${_location.x},${_location.y}")
    seedEntry.getOrElse({
      // Empty pastry network
      return
    })

    // At least one pastry node present
    seedEntry.get._actor ! JoinRequest(_hostEntry)
  }


  override def receive: Receive = {
    case IdRequest =>
      sender() ! id

    case JoinRequest(from) =>
      val node: Option[Entry2] = routingLogic(from._id)
      if(node.isEmpty) {
        sender() ! JoinResponseNotOK(_hostEntry)
      } else {
        // Initialize trace
        val trace:Array[Entry2] = Array[Entry2](node.get)
        if(node.get._id == this.id) {
          sender() ! JoinResponseOK(_hostEntry, trace)
        } else {
          //node ! Route("join", id, trace, sender())
          sender() ! "routing...(not implemented)"
        }
      }
      println(_leafSet.getSet.length + " + " +_routingTable.getTable.length + " + " +_neighbourhoodSet.getSet.length)

    case JoinResponseOK(from, trace) =>
      log.info(s"[${id}]Received OK response from ${from}")

      // A suitable node is found which is numerically close
      // can populate leaf set from last node
      trace.last._actor ! LeafSetRequest

      // The first node in the trace is close by proximity hence
      // neighbourhood set can be populated
      trace.head._actor ! NeighbourhoodSetRequest

      if(trace.length == 1){
        // Get the entire routing table and populate routing table
        trace.head._actor ! RoutingTableRequest(_hostEntry, None)
      } else {
        // If intermediate nodes present, query these nodes to get
        // routing table entries
        val middleTrace: Array[Entry2] = trace.slice(1, trace.length - 1)
        for((node, idx) <- middleTrace.zipWithIndex){
          node._actor ! RoutingTableRequest(_hostEntry, Some(idx))
        }
      }
      sendJoinMessage()


    case JoinResponseNotOK(from) =>
      log.info(s"[${id}]Received Not OK response from ${from}")

      // No suitable node found which is numerically close
      // This means that network is sparse
      // Request all sets and update
      // But...
      // As the node is close by proximity metric
      // can just populate neighbourhood set
      sender() ! NeighbourhoodSetRequest(_hostEntry)
      sender() ! LeafSetRequest(_hostEntry)
      sender() ! RoutingTableRequest(_hostEntry, None)
      addNodeToState(from)
      println(_leafSet.getSet.length + " + " +_routingTable.getTable.length + " + " +_neighbourhoodSet.getSet.length)
      sendJoinMessage()

    case JoinComplete(from) =>
      log.info(s"[${id}]Received [JoinComplete] response from ${from}")
      addNodeToState(from)

    case LeafSetRequest(from) =>
      log.info(s"[${id}]Received [LeafSetRequest] from ${from}")
      println(_leafSet.getSet.length + " + " +_routingTable.getTable.length + " + " +_neighbourhoodSet.getSet.length)
      sender() ! LeafSetResponse(_hostEntry, _leafSet.getSet)

    case LeafSetResponse(from, hisLeafSet) =>
      log.info(s"[${id}]Received [LeafSetResponse] from ${from} with ${hisLeafSet.length}")
      updateState(hisLeafSet)

    case NeighbourhoodSetRequest(from) =>
      log.info(s"[${id}]Received [NeighbourhoodSetRequest] from ${from}")
      println(_leafSet.getSet.length + " + " +_routingTable.getTable.length + " + " +_neighbourhoodSet.getSet.length)
      sender() ! NeighbourhoodSetResponse(_hostEntry, _neighbourhoodSet.getSet)

    case NeighbourhoodSetResponse(from, hisNeighbourhoodSet) =>
      log.info(s"[${id}]Received [NeighbourhoodSetResponse] from ${from} with ${hisNeighbourhoodSet.length}")
      updateNeighbourhood(hisNeighbourhoodSet)

    case RoutingTableRequest(from, idx) =>
      log.info(s"[${id}]Received [RoutingTableRequest] from ${from}")
      println(_leafSet.getSet.length + " + " +_routingTable.getTable.length + " + " +_neighbourhoodSet.getSet.length)
      if(idx.isEmpty)
        sender() ! RoutingTableResponse(from, idx, this._routingTable.getTable)
      else
        sender() ! RoutingTableResponse(from, idx, this._routingTable.getTableRow(idx.get))

    case RoutingTableResponse(from, idx, hisRoutingTable) =>
      log.info(s"[${id}]Received [RoutingTableResponse] from ${from} with ${hisRoutingTable.length}")
      updateState(hisRoutingTable, idx)

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
