package pastry

import akka.actor.ActorSystem

object Main {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("pastry")

    val a1 = system.actorOf(PastryNode.props("127.0.0.1", "1111", Location(0,0)), "first-actor")
    Thread.sleep(2000)
    val e1 = Entry2("1234", a1, Location(0,0))
    val a2 = system.actorOf(PastryNode.props("127.0.0.2", e1, "2222", Location(1,0)), "second-actor")
  }
}
