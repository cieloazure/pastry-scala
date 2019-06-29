package pastry

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

// TODO
// Research maintaining lower and higher sets of numbers
// Heaps on both ends
// TODO
// Think about maintaining two sets vs one

/**
  * LeafSet class is useful for maintaining the set numerically closest a host number.
  *
  * An instance of this class maintains two sets internally - lower and higher. The lower sets contains all the elements
  * less than a given host. The higher set contains all the elements higher than a given host. The lower and higher
  * entries are decided based on the compFn. Each set can only contain maximum of `entries` values. This is maintained
  * by BoundedPriorityQUeue.
  * @param host The entry around which the set is partitioned
  * @param entries The maximum entries in the leafSet. Lower and Higher contains half the entries each.
  * @param compFn The function to decide lower and higher between host or given key
  * @tparam Type Generic type around which the set is built.
  */
class LeafSet[Type](val host: Type, val entries: Int, val compFn: (Type, Type) => Int) {
  val _lower = new BoundedPriorityQueue[Type](entries/2, compFn)
  val _higher = new BoundedPriorityQueue[Type](entries/2, compFn)

  /**
    * Get the lowest element in the leafSet
    *
    * @return Option[Type] None if the set is empty or an element if there are elements in the set
    */
  def lowest: Option[Type] = {
    if(_lower.lowest.isDefined) {
      _lower.lowest
    } else {
      Some(host)
    }
  }

  /**
    * Get the highest element in the leafSet
    *
    * @return Option[Type] None if the set is empty or an element if there are elements in the set
    */
  def highest: Option[Type] = {
    if(_higher.highest.isDefined) {
      _higher.highest
    } else {
      Some(host)
    }
  }

  /**
    * Find a node closest to a given key among the leafSet depending on the difference function
    *
    * If the key is out of range of the leafSet, None is returned. If the key is within range,
    * the entry with closest distance is returned. The closest distance is determined by the difference function
    * which determined the distance between the element and key.
    * @param key Argument to which closest entry in the leaf set is to be found
    * @param diffFn Function to determine the distance between the key and element of leaf set
    * @return Option[Type] None in case the key is out of range of leaf set or the leaf set is empty. If the element
    *         is within range, the entry which is closest to the key.
    */
  def getNode(key: Type, diffFn: (Type, Type) => Int)(implicit m: ClassTag[Type]): Option[Type] = {
    val low: Option[Type] = lowest
    val high: Option[Type] = highest

    if(low.isEmpty && high.isEmpty) return None

    // Not in leaf set
    if(low.isDefined && compFn(key, low.get) < 0){
      return None
    }

    if(high.isDefined && compFn(key, high.get) > 0){
      return None
    }

    val leafSet = getSet :+ host
    val minLeafNode = leafSet.map(diffFn(_, key)).zipWithIndex.min
    Some(leafSet(minLeafNode._2))
  }

  // TODO
  // Understand generics, variance, implicit parameters
  /**
    * Get the leafSet
    *
    * Gets the elements from both lower and upper halves of the set.
    *
    * @param m Implicit parameter to return an array of generic type
    * @return Array[Type] The array of elements in the leaf set
    */
  def getSet(implicit  m: ClassTag[Type]): Array[Type] = {
    var array = ArrayBuffer[Type]()
    array ++= _lower
    array ++= _higher
    array.toArray
  }

  /**
    * Update the set with the nodes based on the comparison function
    *
    * This updates the elements in leafSet based on comparison function of the host
    * @param nodes the nodes to be inserted into the set. The side effect of this operation is the
    *              insertion of nodes in the leafSet. If the entries of the lower and upper arrays are full
    *              only the nodes closest to the host are kept
    */
  def updateSet(nodes: Array[Type]): Unit = {
    val compFnBool = (t1: Type, t2: Type) => {
      compFn(t1, t2) match {
        case 0 => false
        case 1 => false
        case _ => true
      }
    }
    val sortedNodes: Array[Type] = nodes.sortWith(compFnBool)
    val (lower, greater) = sortedNodes.partition(e => compFn(e, host) < 0)
    _higher.offerArray(greater)
    _lower.offerArray(lower)
  }
}
