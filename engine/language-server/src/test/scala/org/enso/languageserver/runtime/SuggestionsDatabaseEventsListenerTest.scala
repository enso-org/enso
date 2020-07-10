package org.enso.languageserver.runtime

import java.nio.file.Files
import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.enso.jsonrpc.test.{FlakySpec, RetrySpec}
import org.enso.languageserver.capability.CapabilityProtocol.{
  AcquireCapability,
  CapabilityAcquired
}
import org.enso.languageserver.data.{
  CapabilityRegistration,
  ReceivesSuggestionsDatabaseUpdates
}
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.session.SessionRouter.DeliverToJsonController
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.searcher.SuggestionsRepo
import org.enso.searcher.sql.SqlSuggestionsRepo
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class SuggestionsDatabaseEventsListenerTest
    extends TestKit(ActorSystem("TestSystem"))
    with ImplicitSender
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with FlakySpec
    with RetrySpec {

  import system.dispatcher

  val Timeout: FiniteDuration = 5.seconds

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "SuggestionsHandler" should {

    "subscribe to notification updates" taggedAs Retry() in withDb {
      (router, repo) =>
        val handler  = newEventsListener(router.ref, repo)
        val clientId = UUID.randomUUID()

        handler ! AcquireCapability(
          newJsonSession(clientId),
          CapabilityRegistration(ReceivesSuggestionsDatabaseUpdates())
        )
        expectMsg(CapabilityAcquired)
    }

    "receive runtime updates" taggedAs Flaky in withDb { (router, repo) =>
      val handler  = newEventsListener(router.ref, repo)
      val clientId = UUID.randomUUID()

      // acquire capability
      handler ! AcquireCapability(
        newJsonSession(clientId),
        CapabilityRegistration(ReceivesSuggestionsDatabaseUpdates())
      )
      expectMsg(CapabilityAcquired)

      // receive updates
      handler ! Api.SuggestionsDatabaseUpdateNotification(
        Suggestions.all.map(Api.SuggestionsDatabaseUpdate.Add)
      )

      val updates = Suggestions.all.zipWithIndex.map {
        case (suggestion, ix) =>
          SearchProtocol.SuggestionsDatabaseUpdate.Add(ix + 1L, suggestion)
      }
      router.expectMsg(
        DeliverToJsonController(
          clientId,
          SearchProtocol.SuggestionsDatabaseUpdateNotification(updates, 4L)
        )
      )

      // check database entries exist
      val (_, records) = Await.result(repo.getAll, Timeout)
      records.map(_.suggestion) should contain theSameElementsAs Suggestions.all
    }

    "apply runtime updates in correct order" taggedAs Flaky in withDb {
      (router, repo) =>
        val handler  = newEventsListener(router.ref, repo)
        val clientId = UUID.randomUUID()

        // acquire capability
        handler ! AcquireCapability(
          newJsonSession(clientId),
          CapabilityRegistration(ReceivesSuggestionsDatabaseUpdates())
        )
        expectMsg(CapabilityAcquired)

        // receive updates
        handler ! Api.SuggestionsDatabaseUpdateNotification(
          Suggestions.all.map(Api.SuggestionsDatabaseUpdate.Add) ++
          Suggestions.all.map(Api.SuggestionsDatabaseUpdate.Remove)
        )

        val updates = Suggestions.all.zipWithIndex.map {
          case (suggestion, ix) =>
            SearchProtocol.SuggestionsDatabaseUpdate.Add(ix + 1L, suggestion)
        }
        router.expectMsg(
          DeliverToJsonController(
            clientId,
            SearchProtocol.SuggestionsDatabaseUpdateNotification(updates, 4L)
          )
        )

        // check that database entries removed
        val (_, all) = Await.result(repo.getAll, Timeout)
        all.map(_.suggestion) should contain theSameElementsAs Suggestions.all
    }

  }

  def newEventsListener(
    router: ActorRef,
    repo: SuggestionsRepo[Future]
  ): ActorRef =
    system.actorOf(SuggestionsDatabaseEventsListener.props(router, repo))

  def newJsonSession(clientId: UUID): JsonSession =
    JsonSession(clientId, TestProbe().ref)

  def withDb(
    test: (TestProbe, SuggestionsRepo[Future]) => Any
  ): Unit = {
    val dbPath = Files.createTempFile("suggestions", ".db")
    system.registerOnTermination(Files.deleteIfExists(dbPath))
    val repo = SqlSuggestionsRepo()
    Await.ready(repo.init, Timeout)
    val router = TestProbe("sessionRouterProbe")

    try test(router, repo)
    finally repo.close()
  }
}
