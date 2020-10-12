package org.enso.projectmanager.infrastructure.languageserver

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.miguno.akka.testing.VirtualTime
import org.enso.projectmanager.data.Socket
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerProtocol.{
  ProjectRenamed,
  RenameFailure,
  RenameTimeout
}
import org.enso.projectmanager.infrastructure.languageserver.ProgrammableWebSocketServer.ReplyWith
import org.enso.projectmanager.infrastructure.net.Tcp
import org.enso.testkit.FlakySpec
import org.mockito.MockitoSugar
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.must.Matchers

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
    val probe = TestProbe()
    fakeServer.withBehaviour {
      case RenameRequestMatcher(requestId, oldName, newName) =>
        probe.ref ! (oldName -> newName)
        ReplyWith(
          s"""{ "jsonrpc": "2.0", "id": "$requestId", "result": null }"""
        )

    }
    probe.expectNoMessage()
    val deathWatcher = TestProbe()
    //when
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
    deathWatcher.watch(actorUnderTest)
    //then
    sender.expectMsg(ProjectRenamed)
    probe.expectMsg(OldName -> NewName)
    deathWatcher.expectTerminated(actorUnderTest)
    //teardown
    fakeServer.stop()
  }

  it should "reply with an error when renaming fails" in new TestCtx {
    //given
    fakeServer.withBehaviour {
      case RenameRequestMatcher(requestId, _, _) =>
        ReplyWith(
          s"""{
             |  "jsonrpc": "2.0",
             |  "id": "$requestId",
             |  "error": {
             |    "code": 100,
             |    "message": "Test"
             |  }
             |}""".stripMargin
        )

    }
    //when
    val deathWatcher = TestProbe()
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
    deathWatcher.watch(actorUnderTest)
    //then
    sender.expectMsg(RenameFailure(100, "Test"))
    deathWatcher.expectTerminated(actorUnderTest)
    //teardown
    fakeServer.stop()
  }

  it should "time out when a server doesn't reply on time" in new TestCtx {
    //when
    val deathWatcher = TestProbe()
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
    deathWatcher.watch(actorUnderTest)
    virtualTimeAdvances(10.seconds)
    //then
    Thread.sleep(5000)
    sender.expectMsg(RenameTimeout)
    deathWatcher.expectTerminated(actorUnderTest)
    //teardown
    fakeServer.stop()
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

    val OldName = "Foo"

    val NewName = "Bar"

    def virtualTimeAdvances(step: FiniteDuration): Unit = {
      //I need to wait some time to give actors time to reply
      Thread.sleep(1000)
      virtualTime.advance(step)
    }

  }
}
