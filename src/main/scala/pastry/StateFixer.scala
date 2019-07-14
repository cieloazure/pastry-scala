package pastry

import akka.actor.{Actor, ActorRef, Props, ReceiveTimeout}

import scala.concurrent.duration._
import scala.util.Random

object StateFixer {
  def props(failedNode: Entry,
            state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]),
            host: ActorRef): Props = Props(new StateFixer(failedNode, state, host))
}

class StateFixer(
                  failedNode: Entry,
                  state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]),
                  host: ActorRef
                ) extends Actor {
  import Node._

  override def preStart(): Unit = {
    fixLeaf(failedNode, Array[Entry](), state, host)
  }

  /**
    * Step 1:Fix leaf set first
    * @param failedNode
    * @param otherFailedNodes
    * @param state
    * @param host
    */
  def fixLeaf(
               failedNode: Entry,
               otherFailedNodes: Array[Entry],
               state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]),
               host: ActorRef
             ): Unit = {

    val partition = state._1.inWhichPartition(failedNode)
    val extremeLeaf = partition match {
      case Some(-1) =>
        Some(state._1.getLower.head)

      case Some(1) =>
        Some(state._1.getHigher.last)

      case _ =>
        None
    }

    if(extremeLeaf.isDefined) {
      val newState = purgeElem(state, failedNode)
      val failedNodes = otherFailedNodes :+ failedNode
      context.setReceiveTimeout(5 minutes)
      context.become(waitForLeafSetResponse(extremeLeaf.get, failedNode, failedNodes, newState, host))
      extremeLeaf.get._actor ! LeafSetRequest
    } else {
      fixRoutingTableRow(failedNode, otherFailedNodes, state, host)
    }
  }

  /**
    * Step2: wait for leaf responses
    * @param waitingForLeaf
    * @param failedNode
    * @param failedNodes
    * @param state
    * @param host
    * @return
    */
  def waitForLeafSetResponse(
                              waitingForLeaf: Entry,
                              failedNode: Entry,
                              failedNodes: Array[Entry],
                              state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]),
                              host: ActorRef
                            ): Receive = {

    case LeafSetResponse(hisLeafSet) =>
      // don't include any failedNodes
      val consideredOnlineNodes = hisLeafSet.filter(!failedNodes.contains(_))
      // check where hisLeafSet overlaps with myLeafSet and update myLeafSet
      val newLeafSet = state._1.update(consideredOnlineNodes)
      // set new state
      val newState = (newLeafSet, state._2, state._3)
      // move to fixing the routing table
      fixRoutingTableRow(failedNode, failedNodes, newState, host)

    case ReceiveTimeout =>
      // purge waitingForLeaf from leafSet of myLeafSet
      val newState = purgeElem(state, waitingForLeaf)
      // fixLeaf()
      fixLeaf(waitingForLeaf, failedNodes, newState, host)
  }

  /**
    * Step 3: Fix routing table
    * @param failedNode
    * @param state
    * @param host
    */
  def fixRoutingTableRow(
                          failedNode: Entry,
                          otherFailedNodes: Array[Entry],
                          state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]),
                          host: ActorRef
                        ): Unit = {

    val row = state._2.inWhichRow(failedNode)
    val oldElems = row match {
      case Some(idx) =>
        Some(state._2.getAllElemFromRow(idx))

      case _ =>
        None
    }
    val activeElems = oldElems.get.filter(_ != failedNode)

    if(activeElems.length > 0 && row.isDefined) {
      val randIdx = Random.nextInt(activeElems.length)
      val randEntry = activeElems(randIdx)
      val failedNodes = otherFailedNodes :+ failedNode
      context.setReceiveTimeout(5 minutes)
      context.become(waitForRoutingTableRowResponse(randEntry, failedNodes, state, host))
      randEntry._actor ! RoutingTableRowRequest(row.get)
    } else {
      context.parent ! StateFixed
      context.stop(self)
    }
  }


  /**
    * Step 4: Check if routing table is fixed
    * @param state
    * @param host
    * @return
    */
  def waitForRoutingTableRowResponse(
                                      waitingForRowEntry: Entry,
                                      failedNodes: Array[Entry],
                                      state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]),
                                      host: ActorRef
                                    ): Receive = {
    case RoutingTableRowResponse(idx, hisRoutingTableRow) =>
      // make sure it is not one of the failedNodes
      val consideredOnlineNodes = hisRoutingTableRow.filter(!failedNodes.contains(_))
      // update our state
      val newRoutingTable = state._2.updateRow(consideredOnlineNodes, idx)
      // set new state
      val newState = (state._1, newRoutingTable, state._3)
      // update host's state
      host ! UpdateState(newState)
      // send state fixed to parent
      context.parent ! StateFixed
      // die
      context.stop(self)

    case ReceiveTimeout =>
      // purge entry from routing table row
      val newState = purgeElem(state, waitingForRowEntry)
      // fix routing table again
      fixRoutingTableRow(waitingForRowEntry, failedNodes, newState, host)
  }

  private def purgeElem(state: (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]), elem: Entry):
  (LeafSet[Entry], RoutingTable[Entry], NeighbourhoodSet[Entry]) = {
    val newLeafSet = state._1.removeElem(elem).getOrElse(state._1)
    val newRoutingTable = state._2.removeElem(elem).getOrElse(state._2)
    val newNeighbourhoodSet = state._3.removeElem(elem).getOrElse(state._3)
    (newLeafSet, newRoutingTable, newNeighbourhoodSet)
  }
  override def receive: Receive = Actor.emptyBehavior
}
