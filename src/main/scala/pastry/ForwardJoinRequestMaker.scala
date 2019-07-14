package pastry

import akka.actor.{Actor, ActorRef, Props, ReceiveTimeout}

import scala.concurrent.duration._

object ForwardJoinRequestMaker {
  def props(to: Entry,
            forNode: Entry,
            trace: Array[Entry],
            state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]),
            host: Entry,
            respondTo: ActorRef): Props = Props(new ForwardJoinRequestMaker(to, forNode, trace, state, host, respondTo))
}

class ForwardJoinRequestMaker(to: Entry,
                              forNode: Entry,
                              trace: Array[Entry],
                              state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]),
                              host: Entry,
                              respondTo: ActorRef)
  extends Actor {
  import Node._

  context.setReceiveTimeout(5 minutes)
  to._actor ! ForwardJoinRequest(forNode, trace, respondTo)

  override def receive: Receive = {
    case ForwardJoinRequestAck =>
      context.stop(self)

    case ReceiveTimeout =>
      // Start Fixer immediately  - Fixer will fix the state and give you back a correct state
      context.actorOf(StateFixer.props(to, state, host._actor), "state-fixer")

      // meanwhile we continue to route the request
      // by purging the node in the state
      val newState = purgeElem(state, to)

      // update our host state
      host._actor ! UpdateState(newState)

      // And forwarding join request to self
      val oldTrace = trace.filter(_ != host)
      host._actor ! ForwardJoinRequest(forNode, oldTrace, respondTo)

      // wait for fixer to stop this actor
      context.become(waitForStateFix)
  }

  def waitForStateFix: Receive = {
    case StateFixed =>
      context.stop(self)
  }

  def purgeElem(state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]), elem: Entry):
  (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]) = {
    val newLeafSet = state._1.removeElem(elem).getOrElse(state._1)
    val newRoutingTable = state._2.removeElem(elem).getOrElse(state._2)
    val newNeighbourhoodSet = state._3.removeElem(elem).getOrElse(state._3)
    (newLeafSet, newRoutingTable, newNeighbourhoodSet)
  }
}
