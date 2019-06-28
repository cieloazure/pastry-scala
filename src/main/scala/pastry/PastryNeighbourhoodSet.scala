package pastry

import akka.actor.ActorRef

class PastryNeighbourhoodSet(host: Entry, entries: Int) {
  var _state: Array[Option[Entry]] = Array.fill(entries)(None)

  def getSet: Array[Entry] = {
    val filtered = _state.filter(_.isDefined)
    if(filtered.isEmpty){
      filtered.flatten
    } else {
      filtered.map(_.get)
    }
  }

  def update(nodes: Array[Entry]): Unit = {
    for(node <- nodes){
      // check the distance of the node with host
      // keep the ones which are closest
    }
  }
}
