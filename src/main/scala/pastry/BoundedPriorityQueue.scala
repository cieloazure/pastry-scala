package pastry

// TODO
// Optimize

class BoundedPriorityQueue[Type](size: Int, comparator: (Type, Type) => Boolean)
                                (implicit initElements: Option[List[Type]] = None) extends Iterable[Type]{
  private val _temp_state: List[Type] = initElements.getOrElse(List[Type]()).sortWith(comparator)
  val _state: List[Type] = if(_temp_state.size > size) {
    _temp_state.drop(_temp_state.size - size)
  } else {
    _temp_state
  }


  def offer(elem: Type): BoundedPriorityQueue[Type] = {
    insert(elem)
  }

  def offerArray(elements: Array[Type]): BoundedPriorityQueue[Type] = {
    insertMany(elements)
  }

  def poll: Option[(Type, BoundedPriorityQueue[Type])] = {
    if(_state.isEmpty) return None
    val min: Type = _state.head
    val _newState = _state.drop(1)
    val newInstance = new BoundedPriorityQueue[Type](size, comparator)(Some(_newState))
    Some(min, newInstance)
  }

  def get(idx: Int): Type = {
    _state(idx)
  }

  private def insert(elem: Type): BoundedPriorityQueue[Type] = {
    val _newState = (elem :: _state).distinct
    new BoundedPriorityQueue[Type](size, comparator)(Some(_newState))
  }

  private def insertMany(elems: Array[Type]): BoundedPriorityQueue[Type] = {
    val _newState = (_state ++ elems).distinct
    new BoundedPriorityQueue[Type](size, comparator)(Some(_newState))
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
