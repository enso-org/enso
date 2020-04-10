package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.miguno.akka.testing.VirtualTime
import org.enso.languageserver.boot.LifecycleComponent.ComponentRestarted
import org.enso.languageserver.boot.{LanguageServerConfig, LifecycleComponent}
import org.enso.projectmanager.boot.configuration.SupervisionConfig
import org.enso.projectmanager.infrastructure.http.AkkaBasedWebSocketConnectionFactory
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerController.ServerDied
import org.enso.projectmanager.infrastructure.languageserver.ProgrammableWebSocketServer.{
  Reject,
  ReplyWith
}
import org.enso.projectmanager.infrastructure.languageserver.StepParent.ChildTerminated
import org.enso.projectmanager.infrastructure.net.Tcp
import org.mockito.BDDMockito._
import org.mockito.Mockito._
import org.mockito.MockitoSugar
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.must.Matchers

import scala.concurrent.Future
import scala.concurrent.duration._

class LanguageServerSupervisorSpec
    extends TestKit(ActorSystem("LanguageServerSupervisorSpec"))
    with ImplicitSender
    with AnyFlatSpecLike
    with Matchers
    with BeforeAndAfterAll
    with MockitoSugar {

  "A language supervisor" should "monitor language server by sending ping requests on regular basis" in new TestCtx {
    //given
    val probe = TestProbe()
    fakeServer.withBehaviour {
      case ping @ PingMatcher(requestId) =>
        probe.ref ! ping
        ReplyWith(
          s"""{ "jsonrpc": "2.0", "id": "$requestId", "result": null }"""
        )
    }
    probe.expectNoMessage()
    //when
    virtualTime.advance(testInitialDelay)
    (1 to 10).foreach { _ =>
      probe.expectMsgPF() { case PingMatcher(_) => () }
      virtualTime.advance(testHeartbeatInterval / 2)
      probe.expectNoMessage()
      virtualTime.advance(testHeartbeatInterval / 2)
    }
    //then
    `then`(serverComponent.restart()).shouldHaveNoInteractions()
    //teardown
    parent ! GracefulStop
    parentProbe.expectMsg(ChildTerminated)
    system.stop(parent)
    fakeServer.stop()
  }

  it should "restart server when pong message doesn't arrive on time" in new TestCtx {
    //given
    when(serverComponent.restart())
      .thenReturn(Future.successful(ComponentRestarted))
    val probe               = TestProbe()
    @volatile var pingCount = 0
    fakeServer.withBehaviour {
      case ping @ PingMatcher(requestId) =>
        probe.ref ! ping
        pingCount += 1
        if (pingCount == 5) {
          Reject
        } else {
          ReplyWith(
            s"""{ "jsonrpc": "2.0", "id": "$requestId", "result": null }"""
          )
        }
    }
    probe.expectNoMessage()
    //when
    virtualTime.advance(testInitialDelay)
    (1 to 4).foreach { _ =>
      verifyNoInteractions(serverComponent)
      probe.expectMsgPF() { case PingMatcher(_) => () }
      virtualTime.advance(testHeartbeatInterval / 2)
      probe.expectNoMessage()
      virtualTime.advance(testHeartbeatInterval / 2)
    }
    probe.expectMsgPF() { case PingMatcher(_) => () }
    virtualTime.advance(testHeartbeatTimeout)
    verify(serverComponent, timeout(VerificationTimeout).times(1)).restart()
    virtualTime.advance(testInitialDelay)
    (1 to 10).foreach { _ =>
      verifyNoMoreInteractions(serverComponent)
      probe.expectMsgPF() { case PingMatcher(_) => () }
      virtualTime.advance(testHeartbeatInterval / 2)
      probe.expectNoMessage()
      virtualTime.advance(testHeartbeatInterval / 2)
    }
    //teardown
    parent ! GracefulStop
    parentProbe.expectMsg(ChildTerminated)
    system.stop(parent)
    fakeServer.stop()
  }

  it should "restart server limited number of times" in new TestCtx {
    //given
    when(serverComponent.restart()).thenReturn(Future.failed(new Exception))
    val probe = TestProbe()
    fakeServer.withBehaviour {
      case ping @ PingMatcher(_) =>
        probe.ref ! ping
        Reject
    }
    probe.expectNoMessage()
    //when
    virtualTime.advance(testInitialDelay)
    probe.expectMsgPF(5.seconds) { case PingMatcher(_) => () }
    verifyNoInteractions(serverComponent)
    virtualTime.advance(testHeartbeatTimeout)
    (1 to testRestartLimit).foreach { i =>
      verify(serverComponent, timeout(VerificationTimeout).times(i)).restart()
      //I need to wait some time to give the supervisor time to schedule next
      // restart command
      Thread.sleep(1000)
      virtualTime.advance(testRestartDelay)
    }
    virtualTime.advance(testHeartbeatInterval)
    probe.expectNoMessage()
    verifyNoMoreInteractions(serverComponent)
    //then
    parentProbe.expectMsg(ServerDied)
    parentProbe.expectMsg(ChildTerminated)
    //teardown
    system.stop(parent)
    fakeServer.stop()
  }

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  trait TestCtx {

    val VerificationTimeout = 120000

    val virtualTime = new VirtualTime

    val serverComponent = mock[LifecycleComponent]

    val testHost = "127.0.0.1"

    val testPort = Tcp.findAvailablePort(testHost, 49152, 65535)

    val testInitialDelay = 5.seconds

    val testHeartbeatInterval = 10.seconds

    val testHeartbeatTimeout = 7.seconds

    val testRestartLimit = 3

    val testRestartDelay = 2.seconds

    val fakeServer = new ProgrammableWebSocketServer(testHost, testPort)
    fakeServer.start()

    val serverConfig =
      LanguageServerConfig(testHost, testPort, UUID.randomUUID(), "/tmp")

    val supervisionConfig =
      SupervisionConfig(
        testInitialDelay,
        testHeartbeatInterval,
        testHeartbeatTimeout,
        testRestartLimit,
        testRestartDelay
      )

    val parentProbe = TestProbe()

    val parent = system.actorOf(
      Props(
        new StepParent(
          LanguageServerSupervisor.props(
            serverConfig,
            serverComponent,
            supervisionConfig,
            new AkkaBasedWebSocketConnectionFactory(),
            virtualTime.scheduler
          ),
          parentProbe.ref
        )
      )
    )

  }
}
