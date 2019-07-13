package pastry

import akka.actor.{ActorRef, ActorSystem}
import pastry.StatisticsNode.AvgJoinHopsRequest

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Main {
  def buildNetwork(system: ActorSystem, statActor: ActorRef): Unit = {
    val ips = getIps
    val ids = getIds

    val actorsArray = ArrayBuffer[Entry2]()
    val sign = Array[Int](1, -1)
    var flag = true
    for((ip, id) <- ips.zip(ids)){
      val lat = sign(Random.nextInt(sign.length)) * Random.nextInt(90)
      val long = sign(Random.nextInt(sign.length)) * Random.nextInt(180)
      val loc = Location(lat, long)
      if(flag){
        val actor = system.actorOf(PastryNode.props(ip,id,
          loc, statActor))
        val entry = Entry2(id, actor, loc)
        actorsArray += entry
        flag = false
      }else{
        val actor = system.actorOf(PastryNode.props(ip, actorsArray(Random.nextInt(actorsArray.size)), id,
          loc, statActor))
        val entry = Entry2(id, actor, loc)
        actorsArray += entry
      }
      Thread.sleep(1000)
    }
  }

  private def getIps = {
    val ipPrefix = "127.0.0."
    val ipArray = ArrayBuffer[String]()
    for (counter <- 0 to PastryConstants.NODES) {
      ipArray += ipPrefix + counter.toString
    }
    ipArray.toArray
  }

  private def getIds :Array[String] = {
    val idPrefix = Array[String]("1", "2", "11", "22", "111", "222")
    val idArray = ArrayBuffer[String]()

    for (prefix <- idPrefix) {
      for (_ <- 0 to 7) {
        idArray += prefix + randomIntString(PastryConstants.LENGTH - prefix.length)
      }
    }

    while (idArray.length != PastryConstants.NODES) {
      idArray += randomIntString(PastryConstants.LENGTH)
    }

    idArray.toArray
  }

  def randomIntString(length: Int): String = {
    val digits = getDigitString(PastryConstants.BASE)
    val intString = new StringBuilder("")
    for(x <- 0 until length){
      intString += digits.charAt(Random.nextInt(digits.length))
    }
    intString.toString
  }

  def getDigitString(limit: Int): String = {
    val digits: StringBuilder = new StringBuilder("")
    val (newLimit, rest) = if (limit > 10) {
      (10, limit - 10)
    } else {
      (limit, -1)
    }

    var start = '0'
    for (i <- 0 until newLimit) {
      digits += (start + i).toChar
    }

    if (rest > -1) {
      start = 'a'
      for (i <- 0 to rest) {
        digits += (start + i).toChar
      }
    }
    digits.toString
  }

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("pastry")

    val statActor = system.actorOf(StatisticsNode.props)
    buildNetwork(system, statActor)
    statActor ! AvgJoinHopsRequest

//    val system = ActorSystem("pastry")
//
//    val a1 = system.actorOf(PastryNode.props("127.0.0.1", "1111",
//      Location(0,0)), "first-actor")
//    val e1 = Entry2("1111", a1, Location(0,0))
//    Thread.sleep(2000)
//
//    val a2 = system.actorOf(PastryNode.props("127.0.0.2", e1, "2222",
//      Location(1,0)), "second-actor")
//    val e2 = Entry2("2222", a2, Location(1,0))
//    Thread.sleep(5000)
//
//    val a3 = system.actorOf(PastryNode.props("127.0.0.3", e2, "3333",
//      Location(0,1)), "third-actor")
//    val e3 = Entry2("3333", a3, Location(0,1))
//    Thread.sleep(5000)
//
//    val a4 = system.actorOf(PastryNode.props("127.0.0.4", e3, "2122",
//      Location(-1,0)), "fourth-actor")
//    val e4 = Entry2("2122", a4, Location(-1,0))
//    Thread.sleep(5000)
//
//    val a5 = system.actorOf(PastryNode.props("127.0.0.5", e4, "1322",
//      Location(0,-1)), "fifth-actor")
//    val e5 = Entry2("1322", a5, Location(-1,0))
//    Thread.sleep(5000)
//
//    val a6 = system.actorOf(PastryNode.props("127.0.0.5", e5, "3156",
//      Location(1,1)), "sixth-actor")
//    val e6 = Entry2("3156", a6, Location(-1,0))
//    Thread.sleep(5000)
  }
}
