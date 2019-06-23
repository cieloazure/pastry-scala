package pastry

import akka.actor.ActorRef

case class Entry(_id: PastryNodeId, _actor: ActorRef) extends Ordered[Entry]{
  override def compare(that: Entry): Int = {
    return _id.compare(that._id)
  }

  def id: PastryNodeId = _id
  def actor: ActorRef = _actor
}
