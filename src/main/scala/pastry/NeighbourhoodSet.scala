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
class NeighbourhoodSet[Type](val _host: Type, val _entries: Int, val _distCompFn: (Type, Type) => Int) {
  var _state: BoundedPriorityQueue[Type] = new BoundedPriorityQueue[Type](size = _entries, comparator = _distCompFn)

  /**
    * Get the entire neighbourhood set
    *
    * @param m Implicit class tag parameter
    * @return the set of elements in the neighbourhood
    */
  def getSet(implicit m: ClassTag[Type]): Array[Type] = {
    _state.toArray
  }

  /**
    * Update the set with given elements
    *
    * @param nodes The nodes of the elements to be updated with
    */
  def updateSet(nodes: Array[Type]): Unit = {
    _state.offerArray(nodes)
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
}
