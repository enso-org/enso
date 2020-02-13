package org.enso.gateway

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit}
import org.enso.{Gateway, LanguageServer}
import org.enso.gateway.TestMessage.{Initialize, Shutdown}
import org.enso.gateway.TestNotification.{Exit, Initialized}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class GatewayMessageSpec()
    extends TestKit(ActorSystem("GatewayMessageSpec"))
    with ImplicitSender
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll {

  private val languageServerActorName = "testingLanguageServer"
  private val gatewayActorName        = "testingGateway"
  private val languageServer: ActorRef =
    system.actorOf(LanguageServer.props(null), languageServerActorName)
  protected val gateway: ActorRef =
    system.actorOf(Gateway.props(languageServer), gatewayActorName)

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "Gateway" must {
    "properly handle init/shutdown workflow" in {
      gateway ! Initialize.request
      expectMsg(Initialize.response)

      gateway ! Initialized.notification
      expectNoMessage()

      gateway ! Shutdown.request
      expectMsg(Shutdown.response)

      gateway ! Exit.notification
      expectNoMessage()
    }
  }
}
