package pastry

import akka.actor.{Actor, ActorRef, Props, ReceiveTimeout}
import scala.concurrent.duration._

object RouteRequestActor {
  def props(length: Int, to: Array[ActorRef], from: Entry, state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])): Props = Props(
    new RouteRequestActor(length, to, from, state)
  )
}

class RouteRequestActor(length: Int, to: Array[ActorRef], from: Entry, state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])) extends Actor with StateUpdatable {
  import Node._

  context.setReceiveTimeout(5 minutes)
  context.become(childRoutingTableResponseContext(length, state))

  for((node, idx) <- to.zipWithIndex) {
    node ! RoutingTableRowRequest(idx)
  }

  override def receive: Receive = {
    Actor.emptyBehavior
  }

  def childRoutingTableResponseContext(numRequests: Int,
                                       state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])):
  Receive = {
    case RoutingTableRowResponse(idx, hisRoutingTable) =>
      val requestsLeft = numRequests - 1
      val newState = updateState(state, hisRoutingTable, Some(idx))
      context.parent ! RequestOK(newState)
      if(requestsLeft == 0) {
        context.stop(self)
      } else {
        context.become(childRoutingTableResponseContext(numRequests, newState))
      }

    case ReceiveTimeout =>
      context.parent ! RequestFailed
      context.stop(self)
  }
}
