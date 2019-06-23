//#full-example
package com.example

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import pastry.{PastryNode, PastryNodeId}
import pastry.PastryNode.JoinRequest

import scala.concurrent.duration._
import scala.language.postfixOps

//#test-classes
class PastryNodeTest(_system: ActorSystem)
  extends TestKit(_system)
    with Matchers
    with WordSpecLike
    with BeforeAndAfterAll {
  //#test-classes

  def this() = this(ActorSystem("PastryNodeSpec"))

  override def afterAll: Unit = {
    shutdown(system)
  }

  //#first-test
//  "A Pastry Actor" should {
//    "with empty seed actor should be the first node in the pastry network" in {
//      //#specification-example
//      system.actorOf(PastryNode.props("127.0.0.1"))
//    }
//  }
//
//  it should {
//    "with non empty seed actor it will receive a new node arrival message" in {
//      val testProbe = TestProbe()
//      val pastryNodeActor: ActorRef = system.actorOf(PastryNode.props("127.0.0.1", testProbe.ref))
//      val id: PastryNodeId = new PastryNodeId("127.0.0.1")
//      testProbe.expectMsg(500 millis, JoinRequest(id, pastryNodeActor))
//    }
//  }

  //#first-test
}
//#full-example
