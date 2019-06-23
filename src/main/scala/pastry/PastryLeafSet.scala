package pastry

import akka.actor.ActorRef

class PastryLeafSet(val host: Entry, val entries: Int) {

  var _lower: Array[Option[Entry]] = Array.fill(entries/2)(None)
  var _higher: Array[Option[Entry]] = Array.fill(entries/2)(None)

  private def lowest: Option[Entry] = {
    val filtered = _lower.filter(_.isDefined)
    if(filtered.isEmpty) return None
    Some(filtered.map(_.get).min)
  }

  private def highest: Option[Entry] = {
    val filtered = _higher.filter(_.isDefined)
    if(filtered.isEmpty) return None
    Some(filtered.map(_.get).max)
  }

  def getNode(key: PastryNodeId): Option[Entry] = {
    val low: Option[Entry] = lowest
    val high: Option[Entry] = highest

    if(low.isEmpty && high.isEmpty) return None

    // Not in leaf set
    if(low.isDefined && key < low.get.id){
      return None
    }

    if(high.isDefined && key > high.get.id){
      return None
    }

    // Combine both
    val all: Array[Option[Entry]] = _lower ++ _higher :+ Some(host)
    val minDiffIdx: Int = all
                      .filter(_.isDefined)
                      .map(_.get)
                      .map(_.id.diff(key))
                      .zipWithIndex
                      .min._2

    Some(all(minDiffIdx).get)
  }

  def getSet: Array[Entry] = {
    val _all = _lower ++ _higher
    _all.filter(_.isDefined).map(_.get)
  }
}
