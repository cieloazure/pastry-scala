package pastry

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Random

/**
  * Routing table for use in pastry applications. This table helps to match nodes based on their prefix. It needs a
  * comparison function to keep the closest nodes at highest priority. Also needs a prefix function to check the nodes
  * which share the prefix.
  *
  *
  * @param _host the node around which prefix is matched
  * @param _nodes parameter to decide number of rows required in the table
  * @param _entries parameter to decide number of entries in each row. Think of them as number of columns.
  * @param distCompFn As the table is bounded, it will only maintain the nodes with highest priority decided by the compFn
  * @param prefixFn A function to determine prefix of one type with another
  * @tparam Type A generic type which can be used
  */
class RoutingTable[Type](val _host: Type,
                         val _nodes: Int,
                         val _entries: Int,
                         val distCompFn: (Type, Type) => Boolean,
                         val prefixFn: (Type, Type) => Int)
                        (implicit state: Option[Array[BoundedPriorityQueue[Type]]] = None) {

  val _maxRows: Int = (math.log(_nodes) / math.log(entries)).toInt
  /**
    * _state preserves the rows with prefix defined. Array is 0 based indexing i.e. 0th index contains nodes which
    * contain has matching prefix of length 1 and so on.
    */
  val _state: Array[BoundedPriorityQueue[Type]] = state.getOrElse(
    Array.fill(_maxRows)(new BoundedPriorityQueue[Type](entries, distCompFn))
  )


  /**
    * Get the number of rows of routing table
    *
    * @return number of rows of routing table
    */
  def rows: Int = _maxRows

  /**
    * Get the number of columns of routing table
    *
    * @return number of columns or entries in each row of the routing table
    */
  def entries: Int = _entries

  /**
    * Get a node from routing table which shares a prefix with the key
    * If no such node exists None is returned
    *
    * @param key the key whose prefix is to be checked in the routing table
    * @return None when no prefix is shared or prefix doesn't have any entries in that row of the routing table
    *         A node which shares a prefix with the key
    */
  def query(key: Type)(implicit m: ClassTag[Type]): Option[Type] = {
    // Get common prefix
    val prefix: Int = prefixFn(key, _host)
    if(prefix == 0) return None
    val rowIdx = if(prefix > _maxRows) { // prefix is greater than # of rows
      val findStatus = _state.zipWithIndex.reverse.find(_._1.nonEmpty) // Start from last row and find first non empty row
      if(findStatus.isDefined) {
        findStatus.get._2 // Found a non empty row
      } else {
        -1 // Did not find one
      }
    } else {
      prefix - 1 // prefix is the within our rows range
    }

    if(rowIdx < 0 || _state(rowIdx).isEmpty) return None // routing table does not have a value

    val entryIdx = Random.nextInt(_state(rowIdx).size) // Get a random node from the row which shares the prefix
                                                       // TODO: Check if the closest node in the routing table
                                                       // Can be determined
    Some(_state(rowIdx).get(entryIdx))
  }

  /**
    * Get the entire contents of the table flattened out in an array
    *
    * @param m Implicit class tag parameter
    * @return Array of nodes in the routing table
    */
  def getAllElem(implicit m: ClassTag[Type]): Array[Type] = {
    _state.flatten
  }

  /**
    * Get a specific row from the table. This function is required when the intermediate nodes
    * make a request for a specific row from their routing table as it is the one with whom their prefix match.
    *
    * @param idx The row in table to fetch
    * @param m implicit classtag parameter
    * @return the nodes contained in that row
    */
  def getAllElemFromRow(idx: Int)(implicit m: ClassTag[Type]): Array[Type] = {
    val bdIdx: Int = boundIdx(idx)
    _state(bdIdx).toArray
  }

  /**
    * Routing table contains only _maxRows of rows based on config
    * If prefix match is greater then bound the match to _maxRows - 1
    * to avoid out of range errors.
    *
    * @param idx
    * @return
    */
  private def boundIdx(idx: Int) = {
    val bdIdx = if (idx >= _maxRows) _maxRows - 1 else idx
    bdIdx
  }

  /**
    * Update the routing table with given nodes. This first groups the nodes into their prefixes and updates
    * a row keeping the highest priority nodes only if overflow occurs
    *
    * @param nodes The nodes to update the routing table with
    * @param m Implicit class tag paramter
    */
  def update(nodes: Array[Type])(implicit m: ClassTag[Type]) : RoutingTable[Type] = {
    if(nodes.nonEmpty){
      val container = ArrayBuffer.fill[ArrayBuffer[Type]](_maxRows)(ArrayBuffer[Type]())
      val partitions = nodes.foldLeft(container)((acc, elem) => {
        val prefix = prefixFn(_host, elem)
        if(prefix > 0){
          val bound = if(prefix > _maxRows) _maxRows else prefix
          acc(bound - 1) += elem
        }
        acc
      })

      val _newState = ArrayBuffer[BoundedPriorityQueue[Type]]()
      for((partition, idx) <- partitions.zipWithIndex){
        _newState += _state(idx).offerArray(partition.toArray)
      }
      new RoutingTable[Type](_host, _nodes, _entries, distCompFn, prefixFn)(Some(_newState.toArray))
    }else{
      this
    }
  }

  /**
    * Update the routing table node with the given nodes. This function is required when a node requests from an
    * intermediate node in the trace the nodes it has at a particular prefix
    *
    * @param nodes the nodes with which the specific row is to be updated
    * @param idx the index of the row in the table
    * @param m implicit class tag parameter
    */
  def updateRow(nodes: Array[Type], idx: Int)(implicit m: ClassTag[Type]): RoutingTable[Type] = {
    if(nodes.nonEmpty) {
      val qualifiedNodes = nodes.filter(prefixFn(_host, _) >= idx)
      val bdIdx: Int = boundIdx(idx)
      val _newState = ArrayBuffer[BoundedPriorityQueue[Type]]()
      for((row, idx) <- _state.zipWithIndex) {
        if(idx != bdIdx) {
          _newState += row
        } else {
          _newState += row.offerArray(nodes)
        }
      }
      new RoutingTable[Type](_host, _nodes, _entries, distCompFn, prefixFn)(Some(_newState.toArray))
    } else {
      this
    }
  }


  /**
    * In which row of the routing table does the element belong
    * @param elem
    * @return
    */
  def inWhichRow(elem: Type)(implicit m: ClassTag[Type]): Option[Int] = {
    val status: Option[(BoundedPriorityQueue[Type], Int)] = _state.zipWithIndex.find(queue => queue._1.exists(_ == elem))
    if(status.isDefined) {
      Some(status.get._2)
    } else {
      None
    }
  }

  /**
    * Remove element from the routing table if it exists in it.
    *
    * @param elem
    * @return
    */
  def removeElem(elem: Type)(implicit m: ClassTag[Type]): Option[RoutingTable[Type]] = {
    inWhichRow(elem) match {
      case None =>
        None

      case Some(idx) =>
        val _row: Array[Type] = getAllElemFromRow(idx)
        val _newRowElems = _row.filter(_ != elem).toList
        val _newRow = new BoundedPriorityQueue[Type](entries, distCompFn)(Some(_newRowElems))
        val _newState = _state.updated(idx, _newRow)
        val _newRoutingTable = new RoutingTable[Type](_host, _nodes, _entries, distCompFn, prefixFn)(Some(_newState))
        Some(_newRoutingTable)
    }
  }
}
