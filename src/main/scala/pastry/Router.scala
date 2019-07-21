package pastry

import akka.actor.{Actor, Props}

object Router {
  def props(state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])): Props = Props(new Router(state))
}

class Router(state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]))
  extends Actor with StateUpdatable {
  import Node._


  def messageHandler(state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])): Receive = {

    case HandleJoinRequest(host, forNode, respondTo) =>
      val trace = makeArray(host)
      val node = routingAlgorithm(forNode._id, state, host)
      println(s"[${host._id}] Routing algorithm found node ${if(node.isDefined) node.get._id else node}")
      if(node.isEmpty || trace.contains(node.get)) {
        respondTo ! JoinRequestOK(trace)
      } else {
        // spawn actor for forward join request
         context.actorOf(ForwardJoinRequestMaker.props(node.get, forNode, trace, state, host, respondTo),
           "forward-join-request-maker")
      }

    case HandleForwardJoinRequest(host, forNode, trace, respondTo) =>
      val newTrace = trace :+ host
      val node = routingAlgorithm(forNode._id, state, host)
      println(s"[${host._id}] Routing algorithm found node ${if(node.isDefined) node.get._id else node}")
      if(node.isEmpty || newTrace.contains(node.get)) {
        respondTo ! JoinRequestOK(newTrace)
      } else {
        // spawn actor for forward join request
        context.actorOf(ForwardJoinRequestMaker.props(node.get, forNode, newTrace, state, host, respondTo),
          "forward-join-request-maker")
      }


    case HandleRouteRequest(host, key, respondTo) =>
      val trace = makeArray(host)
      val node = routingAlgorithm(key, state, host)
      println(s"[${host._id}] Routing algorithm found node ${if(node.isDefined) node.get._id else node}")
      if(node.isEmpty){
        // error
      } else {
        if(trace.contains(node.get)) {
          respondTo ! RouteRequestOK(trace)
        } else {
          context.actorOf(ForwardRouteRequestMaker.props(node.get, key, trace, state, host, respondTo),
            "forward-route-request-maker")
        }
      }

    case HandleForwardRouteRequest(host, key, trace, respondTo) =>
      val newTrace = trace :+ host
      val node = routingAlgorithm(key, state, host)
      println(s"[${host._id}] Routing algorithm found node ${if(node.isDefined) node.get._id else node}")
      if(node.isEmpty){
        // error
      } else {
        if(newTrace.contains(node.get)) {
          respondTo ! RouteRequestOK(newTrace)
        } else {
          context.actorOf(ForwardRouteRequestMaker.props(node.get, key, newTrace, state, host, respondTo),
            "forward-route-request-maker")
        }
      }



    case UpdateState(newState) =>
      context.become(messageHandler(newState))
  }


  def routingAlgorithm(key: String,
                       state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]), host: Entry):
  Option[Entry] = {
    // use routingTable, leafSet and neighbourhoodSet for routing and get appropriate node
    // dummy entry node
    val keyEntry = Entry(key, null, null)

    // Check leafSet
    val leafNode = state._1.query(keyEntry)
    if (leafNode.isDefined) {
      return Some(leafNode.get)
    }

    // Check routingTable
    val routingNode = state._2.query(keyEntry)
    if (routingNode.isDefined) {
      return Some(routingNode.get)
    }

    // Rare case
    // Take union of all
    val combinedState: Array[Entry] = getUnion(state)
    if (combinedState.isEmpty) {
      return None
    }

    val minCommonPrefix = _prefixFn(keyEntry, host)

    //filter out combinedState nodes which have common prefix less than current node's id
    val filtered: Array[Entry] = combinedState
      .filter(_prefixFn(keyEntry, _) >= minCommonPrefix)
      .filter(_._id != host._id)
    if(filtered.isEmpty){
      return None
    }

    //if any nodes are remaining try to find a node whose difference is less than current node
    val minIdx: Int = filtered.map(_diffFn(_, keyEntry)).zipWithIndex.min._2

    Some(filtered(minIdx))
  }
  private def getUnion(state:(LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])):
  Array[Entry] = (state._1.getAllElem ++ state._2.getAllElem ++ state._3.getAllElem).distinct
  private def _prefixFn: (Entry, Entry) => Int = (e1: Entry, e2: Entry) => {
    e1.findCommonPrefix(e2)
  }
  private def _diffFn: (Entry, Entry) => Int = (e1: Entry, e2: Entry) => {
    (e1.toInt(PastryConstants.BASE) - e2.toInt(PastryConstants.BASE)).abs
  }
  private def makeArray(elem: Entry): Array[Entry] = Array[Entry](elem)
  override def receive: Receive = messageHandler(state)
}
