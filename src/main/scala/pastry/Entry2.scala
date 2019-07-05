package pastry

import akka.actor.ActorRef
import util.control.Breaks._

case class Entry2(_id: String, _actor: ActorRef, _location: Location){
  def findCommonPrefix(that: Entry2): Int = {
    var prefix = 0
    breakable {
      for(pair <- this._id.zip(that._id)){
        if(pair._1 == pair._2) {
          prefix += 1
        } else {
          break
        }
      }
    }
    prefix
  }

  def toInt(base: Int): Int = {
    val limit = base
    val digits: StringBuilder = new StringBuilder("")
    val (newLimit, rest) = if(limit > 10){
      (10, limit - 10)
    }else{
      (limit, -1)
    }

    var start = '0'
    for(i <- 0 to newLimit){
      digits += (start + i).toChar
    }

    if(rest > -1){
      start = 'a'
      for(i <- 0 to rest){
        digits += (start + i).toChar
      }
    }

    _id.toList.map(digits.indexOf(_)).reduceLeft(_ * limit + _)
  }
}

