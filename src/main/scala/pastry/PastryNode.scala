package pastry

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

object PastryNode{
  def props(myIpAddress: String, seedActor: ActorRef): Props = Props(new PastryNode(myIpAddress, Some(seedActor)))
  def props(myIpAddress: String): Props = Props(new PastryNode(myIpAddress))
  case class Join(id: PastryNodeId, newActorRef: ActorRef)
}
class PastryNode(val myIpAddress: String, val seedActor: Option[ActorRef] = None) extends Actor with ActorLogging{
  import PastryNode._

  val _id = new PastryNodeId(myIpAddress, 2)
  var _routingTable = new PastryRoutingTable(PastryConstants.NODES, PastryConstants.TWO_RAISED_TO_BASE)
  var _leafSet = new PastryLeafSet(PastryConstants.TWO_RAISED_TO_BASE)
  var _neighbourhoodSet = new PastryNeighbourhoodSet(PastryConstants.TWO_RAISED_TO_BASE)

  def id:PastryNodeId = _id

  override def preStart(): Unit = {
    seedActor.getOrElse({
      // Empty pastry network
      return
    })
    // At least one pastry node present
    seedActor.get ! Join(_id, self)
  }

  // def forward()
  // def deliver()
  // def route()

  override def receive: Receive = {
    case Join(id, newActorRef) =>
      newActorRef ! s"Welcome to pastry! ${id.getHex} in hex or ${id.getIntBase10} in integer"
      // invoke routing logic

//    case Route(message, key) =>
      // Routing logic
      // 1. check leaf set
      // 2. check routing table
      // 3. else take union and route to closest prefix

    case "Thank you!" =>
      log.info("Received:{}", "Thank you!")
      None

    case msg: String =>
      log.info("Received:{}", msg)
      sender() ! "Thank you!"
  }

  // def distance(other: Actor[PastryNode])
}
