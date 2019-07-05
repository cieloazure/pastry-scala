package pastry

import akka.actor.ActorRef

import scala.util.Random

case class Entry(_id: PastryNodeId, _actor: ActorRef, _distance: Option[Int] = None) extends Ordered[Entry]{
  def calcDistance(_hostEntry: Entry) : Int= {
    // TODO
    // send a message to _hostEntry._actor
    // calculate how many hops it took in the network
    // return that distance
    if(_distance.isDefined){
      _distance.get
    } else {
      Random.nextInt(20)
    }
  }

  override def compare(that: Entry): Int = {
    _id.compare(that._id)
  }

  def id: PastryNodeId = _id
  def actor: ActorRef = _actor
}
