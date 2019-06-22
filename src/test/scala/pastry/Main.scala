package pastry

import akka.actor.ActorSystem

object Main {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("pastry")
    val actor1 = system.actorOf(PastryNode.props("127.0.0.1"))
    val actor2 = system.actorOf(PastryNode.props("127.0.0.2", actor1))
  }
}
