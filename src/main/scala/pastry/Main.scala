package pastry

import akka.actor.ActorSystem

object Main {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("pastry")

    val a1 = system.actorOf(PastryNode.props("127.0.0.1", "1111", Location(0,0)), "first-actor")
    val e1 = Entry2("1111", a1, Location(0,0))
    Thread.sleep(2000)

    val a2 = system.actorOf(PastryNode.props("127.0.0.2", e1, "2222", Location(1,0)), "second-actor")
    val e2 = Entry2("2222", a2, Location(1,0))
    Thread.sleep(5000)

    val a3 = system.actorOf(PastryNode.props("127.0.0.3", e2, "3333", Location(0,1)), "third-actor")
    val e3 = Entry2("3333", a2, Location(0,1))
    Thread.sleep(2000)
  }
}
