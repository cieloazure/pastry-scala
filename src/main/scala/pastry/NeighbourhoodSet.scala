package pastry

import scala.reflect.ClassTag

/**
  * Neighbourhood set to maintain nodes close to given host
  *
  * @param _host Given host
  * @param _entries Number of neighobours to maintain
  * @param _distCompFn Comparison function to only keep the top neighbours
  * @tparam Type Type of elements in the set
  */
class NeighbourhoodSet[Type](val _host: Type,
                             val _entries: Int,
                             val _distCompFn: (Type, Type) => Boolean)
                            (implicit state: Option[BoundedPriorityQueue[Type]] = None) {

  var _state: BoundedPriorityQueue[Type] = state.getOrElse(
    new BoundedPriorityQueue[Type](size = _entries, comparator = _distCompFn)
  )

  /**
    * Get the entire neighbourhood set
    *
    * @param m Implicit class tag parameter
    * @return the set of elements in the neighbourhood
    */
  def getAllElem(implicit m: ClassTag[Type]): Array[Type] = {
    _state.toArray
  }

  /**
    * Update the set with given elements
    *
    * @param nodes The nodes of the elements to be updated with
    */
  def update(nodes: Array[Type])(implicit m: ClassTag[Type]): NeighbourhoodSet[Type] = {
    val _validNodes = nodes.filter(_ != _host)
    val _newState = _state.offerArray(_validNodes)
    new NeighbourhoodSet[Type](_host, _entries, _distCompFn)(Some(_newState))
  }

  /**
    * Check if the neighbourhood set is empty
    *
    * @return Boolean indicating true or false
    */
  def isEmpty: Boolean = {
    _state.isEmpty
  }

  /**
    * Check the size of the neighbourhood set
    *
    * @return The size of the internal state
    */
  def size: Int = {
    _state.size
  }

  def getElem(key: Type)(implicit m: ClassTag[Type]): Option[Type] = {
    None
  }


  /**
    *
    * @param elem
    * @return
    */
  def removeElem(elem: Type)(implicit m: ClassTag[Type]): Option[NeighbourhoodSet[Type]] = {
    val oldElems = _state.toArray
    val newElems = oldElems.filter(_ != elem)
    if(newElems.length == oldElems.length) {
      None
    } else {
      val _newState = new BoundedPriorityQueue[Type](newElems.size, _distCompFn)(Some(newElems.toList))
      Some(new NeighbourhoodSet[Type](_host, _entries, _distCompFn)(Some(_newState)))
    }
  }

}
