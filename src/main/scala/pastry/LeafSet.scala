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
  * @param distCompFn function to decide lower and higher between host or given key
  * @param numericCompFn function to decide lower and higher
  * @tparam Type Generic type around which the set is built.
  */
class LeafSet[Type](val host: Type,
                    val entries: Int,
                    val distCompFn: (Type, Type) => Boolean,
                    val numericCompFn: (Type, Type) => Boolean,
                    val diffFn: (Type, Type) => Int)
                   (implicit lower: Option[BoundedPriorityQueue[Type]] = None,
                    higher: Option[BoundedPriorityQueue[Type]] = None) {
  val _lower: BoundedPriorityQueue[Type] = lower.getOrElse(new BoundedPriorityQueue[Type](entries/2, distCompFn))
  val _higher: BoundedPriorityQueue[Type] = higher.getOrElse(new BoundedPriorityQueue[Type](entries/2, distCompFn))

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
    * @return Option[Type] None in case the key is out of range of leaf set or the leaf set is empty. If the element
    *         is within range, the entry which is closest to the key.
    */
  def query(key: Type)(implicit m: ClassTag[Type]): Option[Type] = {
    val low: Option[Type] = lowest
    val high: Option[Type] = highest

    if(low.isEmpty && high.isEmpty) return None

    // Not in leaf set
    if(low.isDefined && numericCompFn(key, low.get)){
      return None
    }

    if(high.isDefined && numericCompFn(high.get, key)){
      return None
    }

    val leafSet = getAllElem :+ host
    val mapping = leafSet.map(diffFn(_, key)).zipWithIndex
    val minLeafNode = mapping.min
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
  def getAllElem(implicit  m: ClassTag[Type]): Array[Type] = {
    var buffer = ArrayBuffer[Type]()
    buffer ++= _lower
    buffer += host
    buffer ++= _higher
    buffer.toArray
  }

  /**
    * Update the set with the nodes based on the comparison function
    *
    * This updates the elements in leafSet based on comparison function of the host
    * @param nodes the nodes to be inserted into the set. The side effect of this operation is the
    *              insertion of nodes in the leafSet. If the entries of the lower and upper arrays are full
    *              only the nodes closest to the host are kept
    */
  def update(nodes: Array[Type])(implicit m: ClassTag[Type]): LeafSet[Type] = {
    val validNodes = nodes.filter(_ != host)
    val (lower, greater) = validNodes.partition(e => numericCompFn(e, host))
    val _newLower: BoundedPriorityQueue[Type] = _lower.offerArray(lower)
    val _newHigher: BoundedPriorityQueue[Type] = _higher.offerArray(greater)
    new LeafSet[Type](host, entries, distCompFn, numericCompFn, diffFn)(Some(_newLower), Some(_newHigher))
  }

  /**
    * In which partition - lower or upper - does the element belong
    * Useful to query the extreme
    *
    * @param elem
    * @return
    */
  def inWhichPartition(elem: Type): Option[Int] = {
    if(elem == host){
      Some(0)
    } else if(_lower.exists(_ == elem)){
      Some(-1)
    } else if(_higher.exists(_ == elem)){
      Some(1)
    } else {
      None
    }
  }

  /**
    * Remove the element from the leafSet and return a new leafSet
    *
    * @param elem
    * @return
    */
  def removeElem(elem: Type): Option[LeafSet[Type]] = {
    inWhichPartition(elem) match {
      case Some(0) =>
        None

      case None =>
        None

      case Some(-1) =>
        val _init_lower = _lower.filter(_ != elem).toList
        val _newLower = new BoundedPriorityQueue[Type](_init_lower.size, distCompFn)(Some(_init_lower))
        val _newLeafSet = new LeafSet[Type](host, entries - 1, distCompFn, numericCompFn, diffFn)(Some(_newLower), Some(_higher))
        Some(_newLeafSet)

      case Some(1) =>
        val _init_higher = _higher.filter(_ != elem).toList
        val _newHigher = new BoundedPriorityQueue[Type](_init_higher.size, distCompFn)(Some(_init_higher))
        val _newLeafSet = new LeafSet[Type](host, entries - 1, distCompFn, numericCompFn, diffFn)(Some(_lower), Some(_newHigher))
        Some(_newLeafSet)
    }
  }

  /**
    * Get the lower half of the leafSet
    *
    * @return
    */
  def getLower(implicit m: ClassTag[Type]): Array[Type] = {
    var buffer = ArrayBuffer[Type]()
    buffer ++= _lower
    buffer.toArray
  }

  /**
    * Get the upper half of the leafSet
    *
    * @return
    */
  def getHigher(implicit m: ClassTag[Type]): Array[Type] = {
    var buffer = ArrayBuffer[Type]()
    buffer ++= _higher
    buffer.toArray
  }
}
