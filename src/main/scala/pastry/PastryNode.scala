package pastry

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import pastry.PastryNode.NewNodeArrival

object PastryNode{
  def props(myIpAddress: String, seedActor: ActorRef): Props = Props(new PastryNode(myIpAddress, Some(seedActor)))
  def props(myIpAddress: String): Props = Props(new PastryNode(myIpAddress))
  case class NewNodeArrival(newActorRef: ActorRef)
}
class PastryNode(val myIpAddress: String, val seedActor: Option[ActorRef] = None) extends Actor with ActorLogging{
  val _id = new PastryNodeId(myIpAddress, 2)

  def id = _id

  override def preStart(): Unit = {
    log.info("Starting....")
    log.info("Before that checking if the seedActor is empty....")
    seedActor.getOrElse({
      log.info("Empty.....We are the first node in the pastry network")
      return
    })
    log.info("Not empty....there are other pastry nodes existing passed in as argument")
    log.info("Try to connect to pastry node actor....")
    log.info("If connection successful, create a node id and ask Pastry Node actor A to route join message")
    log.info("Populate state tables....")

    seedActor.get ! NewNodeArrival(self)
  }

  override def receive: Receive = {
    case NewNodeArrival(newActorRef) =>
      log.info("New pastry node arrived.....")
      newActorRef ! "Welcome"

    case msg: String =>
      log.info("Received:{}", msg)
  }
}
