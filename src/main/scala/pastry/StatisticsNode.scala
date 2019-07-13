package pastry

import akka.actor.{Actor, Props}

object StatisticsNode{
  def props: Props = Props(new StatisticsNode)

  case class JoinStats(hopsRequiredForJoin: Int)
  case object AvgJoinHopsRequest
  case class AvgJoinHopsResponse(avgHops: Int)
}
class StatisticsNode extends Actor {
  import StatisticsNode._

  def statsContext(currentSum: Int, currentRecordings: Int): Receive = {
    case JoinStats(hopsRequiredForJoin) =>
      context.become(statsContext(currentSum + hopsRequiredForJoin, currentRecordings + 1))

    case AvgJoinHopsRequest =>
      println(s"Average hops for join: ${currentSum.toFloat / currentRecordings}")
  }

  override def receive: Receive = statsContext(0, 0)
}
