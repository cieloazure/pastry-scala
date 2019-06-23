package pastry

import akka.actor.ActorRef

object PastryRoutingTable {
  //  case class Entry(id: PastryNodeId,ref: ActorRef )
  case class Entry(id: Int)
}

class PastryRoutingTable(val nodes: Int, val entries: Int) {
  import PastryRoutingTable._

  var _state: Map[Int, Array[Entry]] = Map()
  var _rows: Int = (math.log(nodes) / math.log(entries)).toInt

  for(_rowNum <- 0 to _rows + 1){
    _state = _state + (_rowNum -> Array[Entry]())
  }

   def add(nodes: Array[Entry], prefixLength: Int): Unit = {
     var prevArray: Array[Entry] = _state(prefixLength)
     val updatedArray = prevArray ++ nodes
     _state = _state + (prefixLength -> updatedArray)
  }

  def add(node: Entry, prefixLength: Int): Unit = {
    var prevArray: Array[Entry] = _state(prefixLength)
    var updatedArray = prevArray :+ node
    _state = _state + (prefixLength -> updatedArray)
  }

  def get(prefixLength: Int): Array[Entry] = {
    _state(prefixLength)
  }
}
