package pastry

import akka.actor.{Actor, ActorRef, Props, ReceiveTimeout}
import scala.concurrent.duration._

object RequestActor {
  def props(contextIdx: Int, to: ActorRef, request: Any, state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])): Props = Props(new RequestActor(contextIdx, to, request, state))
}

class RequestActor(contextIdx: Int,
                   to: ActorRef,
                   request: Any,
                   state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])) extends Actor with StateUpdatable {
  import Node._

  context.setReceiveTimeout(5 minutes)

  contextIdx match {
    case 1 => context.become(childJoinStatusContext())
    case 2 => context.become(childLeafSetResponseContext(state))
    case 3 => context.become(childNeighbourhoodSetResponseContext(state))
    case 4 => context.become(childRoutingTableResponseContext(state))
  }

  to ! request

  def childJoinStatusContext():
  Receive = {
    case JoinRequestOK(trace) =>
      context.parent ! JoinRequestOK(trace)
      context.stop(self)

    case ReceiveTimeout =>
      context.parent ! JoinRequestFailed
      context.stop(self)
  }
  def childLeafSetResponseContext(state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])):
  Receive = {
    case LeafSetResponse(hisLeafSet) =>
      val newState = updateState(state, hisLeafSet)
      context.parent ! RequestOK(newState)
      context.stop(self)

    case ReceiveTimeout =>
      context.parent ! RequestFailed
      context.stop(self)
  }
  def childNeighbourhoodSetResponseContext(state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])):
  Receive = {
    case NeighbourhoodSetResponse(from, hisNbhd) =>
      val totalNbhd = hisNbhd :+ from
      val newState = updateNeighbourhood(state, totalNbhd)
      context.parent ! RequestOK(newState)
      context.stop(self)

    case ReceiveTimeout =>
      context.parent ! RequestFailed
      context.stop(self)
  }
  def childRoutingTableResponseContext(state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])):
  Receive = {
    case RoutingTableResponse(hisRoutingTable) =>
      val newState = updateState(state, hisRoutingTable)
      context.parent ! RequestOK(newState)
      context.stop(self)

    case ReceiveTimeout =>
      context.parent ! RequestFailed
      context.stop(self)

    case msg: Any =>
      println(msg)
  }
  private def makeStr(entries: Array[Entry]): String = {
    entries.map(_._id).mkString(",")
  }
  private def printState(prefix: String, state:(LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry])): Unit = {
    println(s"[$prefix] State: ([${makeStr(state._1.getAllElem)}] + [${makeStr(state._2.getAllElem)}] " +
      s"+ [${makeStr(state._3.getAllElem)}])")
  }
  override def receive: Receive = Actor.emptyBehavior
}
