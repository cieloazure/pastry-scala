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
  * @param compFn As the table is bounded, it will only maintain the nodes with highest priority decided by the compFn
  * @param prefixFn A function to determine prefix of one type with another
  * @tparam Type A generic type which can be used
  */
class RoutingTable[Type](val _host: Type, val _nodes: Int, val _entries: Int, val compFn: (Type, Type) => Int, val prefixFn: (Type, Type) => Int) {

  val _maxRows: Int = (math.log(_nodes) / math.log(entries)).toInt
  /**
    * _state preserves the rows with prefix defined. Array is 0 based indexing i.e. 0th index contains nodes which
    * contain has matching prefix of length 1 and so on.
    */
  val _state: Array[BoundedPriorityQueue[Type]] = Array.fill(_maxRows)(new BoundedPriorityQueue[Type](entries, compFn))


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
  def getNode(key: Type): Option[Type] = {
    // Get common prefix
    val prefix: Int = prefixFn(key, _host)
    if(prefix == 0) return None
    val rowIdx = if(prefix > _maxRows) {
      val findStatus = _state.zipWithIndex.reverse.find(_._1.nonEmpty)
      if(findStatus.isDefined) {
        findStatus.get._2
      } else {
        -1
      }
    } else {
      prefix - 1
    }

    if(rowIdx < 0 || _state(rowIdx).isEmpty) return None

    val entryIdx = Random.nextInt(_state(rowIdx).size)
    Some(_state(rowIdx).get(entryIdx))
  }

  /**
    * Get the entire contents of the table flattened out in an array
    *
    * @param m Implicit class tag parameter
    * @return Array of nodes in the routing table
    */
  def getTable(implicit m: ClassTag[Type]): Array[Type] = {
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
  def getTableRow(idx: Int)(implicit m: ClassTag[Type]): Array[Type] = {
    _state(idx).toArray
  }

  /**
    * Update the routing table with given nodes. This first groups the nodes into their prefixes and updates
    * a row keeping the highest priority nodes only if overflow occurs
    *
    * @param nodes The nodes to update the routing table with
    * @param m Implicit class tag paramter
    */
  def updateTable(nodes: Array[Type])(implicit m: ClassTag[Type]) : Unit = {
    val container = ArrayBuffer.fill[ArrayBuffer[Type]](_maxRows)(ArrayBuffer[Type]())
    val partitions = nodes.foldLeft(container)((acc, elem) => {
      val prefix = prefixFn(_host, elem)
      if(prefix > 0){
        container(prefix - 1) += elem
      }
      container
    })
    partitions.drop(1)
    for((partition, idx) <- partitions.zipWithIndex){
      _state(idx).offerArray(partition.toArray)
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
  def updateTableRow(nodes: Array[Type], idx: Int)(implicit m: ClassTag[Type]): Unit = {
    _state(idx).offerArray(nodes)
  }
}
