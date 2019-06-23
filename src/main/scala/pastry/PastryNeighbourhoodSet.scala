package pastry

import akka.actor.ActorRef

class PastryNeighbourhoodSet(entries: Int) {
  var _state: Array[Option[Entry]] = Array.fill(entries)(None)

  def getSet: Array[Entry] = {
    val filtered = _state.filter(_.isDefined)
    if(filtered.isEmpty){
      filtered.flatten
    } else {
      filtered.map(_.get)
    }
  }
}
