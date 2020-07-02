package org.enso.languageserver.runtime

import java.nio.file.Files
import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
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
import org.enso.searcher.sql.{SqlDatabase, SqlSuggestionsRepo}
import org.enso.searcher.{Database, SuggestionsRepo}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import slick.dbio.DBIO

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class SuggestionsDatabaseEventsListenerTest
    extends TestKit(ActorSystem("TestSystem"))
    with ImplicitSender
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll {

  import system.dispatcher

  val Timeout: FiniteDuration = 5.seconds

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "SuggestionsHandler" should {

    "subscribe to notification updates" in withDb { (router, repo, db) =>
      val handler  = newEventsListener(router.ref, repo, db)
      val clientId = UUID.randomUUID()

      handler ! AcquireCapability(
        newJsonSession(clientId),
        CapabilityRegistration(ReceivesSuggestionsDatabaseUpdates())
      )
      expectMsg(CapabilityAcquired)
    }

    "receive runtime updates" in withDb { (router, repo, db) =>
      val handler  = newEventsListener(router.ref, repo, db)
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
      val records = Await.result(db.run(repo.getAll), Timeout)
      records.map(_.suggestion) should contain theSameElementsAs Suggestions.all
    }

    "apply runtime updates in correct order" in withDb { (router, repo, db) =>
      val handler  = newEventsListener(router.ref, repo, db)
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
      val records = Await.result(db.run(repo.getAll), Timeout)
      records.map(_.suggestion) should contain theSameElementsAs Suggestions.all
    }

  }

  def newEventsListener(
    router: ActorRef,
    repo: SuggestionsRepo[DBIO],
    db: Database[DBIO, Future]
  ): ActorRef =
    system.actorOf(SuggestionsDatabaseEventsListener.props(router, repo, db))

  def newJsonSession(clientId: UUID): JsonSession =
    JsonSession(clientId, TestProbe().ref)

  def withDb(
    test: (TestProbe, SuggestionsRepo[DBIO], Database[DBIO, Future]) => Any
  ): Unit = {
    val dbPath = Files.createTempFile("suggestions", ".db")
    system.registerOnTermination(Files.deleteIfExists(dbPath))
    val repo = new SqlSuggestionsRepo()
    val db   = new SqlDatabase()
    Await.ready(db.run(repo.init), Timeout)
    val router = TestProbe("sessionRouterProbe")

    try test(router, repo, db)
    finally db.close()
  }
}
