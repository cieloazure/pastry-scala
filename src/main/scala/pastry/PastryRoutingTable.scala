package pastry

import scala.reflect.ClassTag
import scala.util.Random

class PastryRoutingTable[Type](val host: Type, val nodes: Int, val entries: Int, val compFn: (Type, Type) => Int, val prefixFn: (Type, Type) => Int) {
  val _maxRows: Int = (math.log(nodes) / math.log(entries)).toInt
  val _state: Array[BoundedPriorityQueue[Type]] = Array.fill(_maxRows)(new BoundedPriorityQueue[Type](entries, compFn))

  def getNode(key: Type): Option[Type] = {
    // Get common prefix
    val prefix: Int = prefixFn(key, host)
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

  def getTable(implicit m: ClassTag[Type]): Array[Type] = {
    _state.flatten
  }

  def updateTable(nodes: Array[Type]) : Unit = {
    for(node <- nodes){
      val prefix: Int = prefixFn(host, node)
      if(prefix > 0)_state(prefix - 1).offer(node)
    }
  }
}
