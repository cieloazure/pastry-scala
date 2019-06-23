package pastry

import akka.actor.ActorRef

class PastryNeighbourhoodSet(entries: Int) {
  var _state: Array[ActorRef] = Array[ActorRef]()

  def add(neighbour: ActorRef): Unit = {
    _state = _state :+ neighbour
    if(_state.length > entries) _state.dropRight(1)
  }
}
