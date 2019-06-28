package pastry

// TODO
// Implement with Heaps later

class BoundedPriorityQueue[Type](size: Int, comparator: (Type, Type) => Int) extends Iterable[Type]{
  var _state: List[Type] = List[Type]()

  def offer(elem: Type): Unit = {
    insert(elem)
    bound
  }

  def offerArray(elements: Array[Type]): Unit = {
    for(elem <- elements){
      insert(elem)
    }
    bound
  }

  private def bound: Unit = {
    while(_state.size > size){
      poll
    }
  }

  private def insert(elem: Type): Unit = {
    val position: Option[(Type, Int)] = _state.zipWithIndex.find(e => comparator(e._1, elem) > 0)
    val idx: Int = if(position.isDefined) {
      position.get._2
    } else {
      _state.length
    }

    val (front, back) = _state.splitAt(idx)

    _state = front :+ elem
    _state = _state ++ back
  }

  def poll: Option[Type] = {
    if(_state.isEmpty) return None
    val min: Type = _state.head
    _state = _state.drop(1)
    Some(min)
  }

  def get(idx: Int): Type = {
    _state(idx)
  }

  override def foreach[U](f: Type => U): Unit = {
    _state foreach f
  }

  override def iterator: Iterator[Type] = {
    _state.toIterator
  }

  def lowest: Option[Type] = {
    _state.headOption
  }

  def highest: Option[Type] = {
    _state.lastOption
  }
}
