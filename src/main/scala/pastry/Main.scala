package pastry

import akka.actor.{ActorRef, ActorSystem}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Main {
  def buildNetwork(system: ActorSystem, statActor: ActorRef): Unit = {
    val ips = getIps
    val ids = getIds

    val actorsArray = ArrayBuffer[Entry]()
    val sign = Array[Int](1, -1)
    var flag = true
    for((ip, id) <- ips.zip(ids)){
      val lat = sign(Random.nextInt(sign.length)) * Random.nextInt(90)
      val long = sign(Random.nextInt(sign.length)) * Random.nextInt(180)
      val loc = Location(lat, long)
      if(flag){
        val actor = system.actorOf(Node.props(ip,id,
          loc, statActor))
        val entry = Entry(id, actor, loc)
        actorsArray += entry
        flag = false
      }else{
        val actor = system.actorOf(Node.props(ip, actorsArray(Random.nextInt(actorsArray.size)), id,
          loc, statActor))
        val entry = Entry(id, actor, loc)
        actorsArray += entry
      }
      Thread.sleep(2000)
      bufferSpace
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
    for(_ <- 0 until length){
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

//    val a1: ActorRef = system.actorOf(Node.props("127.0.0.1", "1111", Location(0,0), statActor), "first-actor")
//    val e1 = Entry("1111", a1, Location(0,0))
//
//    Thread.sleep(2000)
//    bufferSpace
//
//    val a2 = system.actorOf(Node.props("127.0.0.2", e1, "2222", Location(0,1), statActor), "second-actor")
//    val e2 = Entry("2222", a2, Location(0,1))
//
//    Thread.sleep(2000)
//    bufferSpace
//
//    val a3 = system.actorOf(Node.props("127.0.0.3", e2, "3333", Location(1,0), statActor), "third-actor")
//    val e3 = Entry("3333", a3, Location(1,0))
//
//    Thread.sleep(2000)
//    bufferSpace
//
//    val a4 = system.actorOf(Node.props("127.0.0.4", e3, "1211", Location(0, -1), statActor), "fourth-actor")
//    val e4 = Entry("1211", a4, Location(0, -1))
//
//    Thread.sleep(2000)
//    bufferSpace
//
//    val a5 = system.actorOf(Node.props("127.0.0.5", e4, "2311", Location(-1, 0), statActor), "fifth-actor")
//    val e5 = Entry("2311", a5, Location(-1, 0))
//
//    Thread.sleep(2000)
//    bufferSpace
//
//    val a6 = system.actorOf(Node.props("127.0.0.6", e4, "3413", Location(1, 1), statActor), "sixth-actor")
//    val e6 = Entry("3413", a6, Location(1, 1))

    import Node._
    buildNetwork(system, statActor)
    Thread.sleep(5000)
    statActor ! AvgJoinHopsRequest
  }

  def bufferSpace: Unit = {
    for(_ <- 1 to 5) {
      println()
    }
  }
}
