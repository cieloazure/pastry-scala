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
    buildNetwork(system, statActor)
    statActor ! AvgJoinHopsRequest
  }
}
