package org.enso.languageserver

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.enso.LanguageServer
import org.enso.languageserver.Notifications.{Exit, Initialized}
import org.enso.languageserver.Requests.{Initialize, Shutdown}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class LanguageServerSpec()
    extends TestKit(ActorSystem("LanguageServerSpec"))
    with ImplicitSender
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll {

  private val languageServerActorName = "testingLanguageServer"
  private val languageServer: ActorRef =
    system.actorOf(LanguageServer.props(null), languageServerActorName)

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "Language Server" must {
    "properly handle init/shutdown workflow" in {
      val probe    = TestProbe()
      val probeRef = probe.ref

      val id1 = Id.Number(1)
      val id2 = Id.Number(2)

      languageServer ! Initialize(id1, probeRef)
      expectMsg(RequestReceived.Initialize(id1, probeRef))

      languageServer ! Initialized
      expectMsg(NotificationReceived.Initialized)

      languageServer ! Shutdown(id2, probeRef)
      expectMsg(RequestReceived.Shutdown(id2, probeRef))

      languageServer ! Exit
      expectMsg(NotificationReceived.Exit)
    }
  }
}
