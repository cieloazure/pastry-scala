package pastry

import akka.actor.{Actor, Props}

object StatisticsNode{
  def props: Props = Props(new StatisticsNode)
}
class StatisticsNode extends Actor {
  import Node._

  def statsContext(currentSum: Int, currentRecordings: Int): Receive = {
    case JoinStats(hopsRequiredForJoin) =>
      context.become(statsContext(currentSum + hopsRequiredForJoin, currentRecordings + 1))

    case AvgJoinHopsRequest =>
      println(s"Average hops for join: ${currentSum.toFloat / currentRecordings}")
  }

  override def receive: Receive = statsContext(0, 0)
}
