package pastry

object PastryLeafSet{
  case class Entry(id: Int)
}
class PastryLeafSet(val entries: Int) {
  import PastryLeafSet._


  var _state: Map[Int, Array[Entry]] = Map[Int, Array[Entry]]()
  _state = _state + (0 -> Array[Entry](), 1 -> Array[Entry]())


  def getLower: Array[Entry] = {
    _state(0)
  }

  def addLower(node: Entry): Unit = {
    val prev = _state(0)
    val curr = node +: prev
    if(curr.length > entries) curr.dropRight(1)
    _state = _state + (0 -> curr)
  }

  def getHigher: Array[Entry] = {
    _state(1)
  }

  def addHigher(node: Entry): Unit = {
    val prev = _state(1)
    var curr = node +: prev
    if(curr.length > entries) curr.dropRight(1)
    _state = _state + (1 -> curr)
  }

  def copy(other: PastryLeafSet): Unit = {
    this._state = other._state
  }
}
