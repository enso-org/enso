package org.enso.projectmanager.infrastructure.languageserver

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.miguno.akka.testing.VirtualTime
import org.enso.jsonrpc.test.FlakySpec
import org.enso.projectmanager.data.Socket
import org.enso.projectmanager.infrastructure.languageserver.ProgrammableWebSocketServer.ReplyWith
import org.enso.projectmanager.infrastructure.net.Tcp
import org.mockito.MockitoSugar
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.must.Matchers

import scala.annotation.unused
import scala.concurrent.duration._

class ProjectRenameActionSpec
    extends TestKit(ActorSystem("LanguageServerSupervisorSpec"))
    with ImplicitSender
    with AnyFlatSpecLike
    with Matchers
    with BeforeAndAfterAll
    with MockitoSugar
    with FlakySpec {

  "A project rename action" should "delegate request to the Language Server" in new TestCtx {
    //given
    val OldName = "Foo"
    val NewName = "Bar"
    val probe   = TestProbe()
    fakeServer.withBehaviour {
      case RenameRequestMatcher(requestId, oldName, newName) =>
        probe.ref ! (oldName -> newName)
        ReplyWith(
          s"""{ "jsonrpc": "2.0", "id": "$requestId", "result": null }"""
        )

    }
    probe.expectNoMessage()
    //when
    @unused
    val actorUnderTest = system.actorOf(
      ProjectRenameAction.props(
        sender.ref,
        Socket(testHost, testJsonPort),
        10.seconds,
        2.seconds,
        OldName,
        NewName,
        virtualTime.scheduler
      )
    )
    //then
    sender.expectMsg(LanguageServerProtocol.ProjectRenamed)
    probe.expectMsg(OldName -> NewName)
  }

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  trait TestCtx {

    val VerificationTimeout = 120000

    val virtualTime = new VirtualTime

    val testHost = "127.0.0.1"

    val testJsonPort = Tcp.findAvailablePort(testHost, 49152, 55535)

    val fakeServer = new ProgrammableWebSocketServer(testHost, testJsonPort)
    fakeServer.start()

    val sender = TestProbe()

    def virtualTimeAdvances(step: FiniteDuration): Unit = {
      //I need to wait some time to give actors time to reply
      Thread.sleep(1000)
      virtualTime.advance(step)
    }

  }
}
