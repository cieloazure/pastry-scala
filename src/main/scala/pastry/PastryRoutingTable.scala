package pastry

import akka.actor.ActorRef

class PastryRoutingTable(val host: Entry, val nodes: Int, val entries: Int) {

  var _rows: Int = (math.log(nodes) / math.log(entries)).toInt
  var _state: Array[Array[Option[Entry]]] = Array.fill(_rows, entries)(None)


  def getNode(key: PastryNodeId): Option[Entry] = {
    // Get common prefix
    val commonPrefixLen: Int = key.findCommonPrefix(host.id)
    if(commonPrefixLen == 0) return None
    val entryIdx: Int = key.getDigit(commonPrefixLen)

    val entry: Entry = _state(commonPrefixLen)(entryIdx).getOrElse({
      return None
    })

    Some(entry)
  }

  def getTable: Array[Entry] = {
    _state.flatten.filter(_.isDefined).map(_.get)
  }

  def update(nodes: Array[Entry]): Unit = {
    for(node <- nodes){
      val prefix: Int = node.id.findCommonPrefix(host.id)
      if(_state(prefix).length == entries){
        _state(prefix).drop(1)
      }
      _state(prefix) = _state(prefix) :+ Some(node)
    }
  }
}
