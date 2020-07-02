package org.enso.languageserver.runtime

import java.nio.file.Files

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit}
import org.enso.searcher.sql.{SqlDatabase, SqlSuggestionsRepo}
import org.enso.searcher.{Database, SuggestionsRepo}
import org.enso.text.editing.model.Position
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike
import slick.dbio.DBIO

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class SuggestionsHandlerTest
    extends TestKit(ActorSystem("TestSystem"))
    with ImplicitSender
    with AnyWordSpecLike
    with BeforeAndAfterAll {

  import system.dispatcher

  val Timeout: FiniteDuration = 10.seconds

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "SuggestionsHandler" should {

    "get initial suggestions database version" in withDb { (repo, db) =>
      val handler = newSuggestionsHandler(repo, db)
      handler ! SearchProtocol.GetSuggestionsDatabaseVersion

      expectMsg(SearchProtocol.GetSuggestionsDatabaseVersionResult(0))
    }

    "get suggestions database version" in withDb { (repo, db) =>
      val handler = newSuggestionsHandler(repo, db)
      Await.ready(db.run(repo.insert(Suggestions.atom)), Timeout)

      handler ! SearchProtocol.GetSuggestionsDatabaseVersion

      expectMsg(SearchProtocol.GetSuggestionsDatabaseVersionResult(1))
    }

    "get initial suggestions database" in withDb { (repo, db) =>
      val handler = newSuggestionsHandler(repo, db)
      handler ! SearchProtocol.GetSuggestionsDatabase

      expectMsg(SearchProtocol.GetSuggestionsDatabaseResult(Seq(), 0))
    }

    "get suggestions database" in withDb { (repo, db) =>
      val handler = newSuggestionsHandler(repo, db)
      Await.ready(db.run(repo.insert(Suggestions.atom)), Timeout)
      handler ! SearchProtocol.GetSuggestionsDatabase

      expectMsg(
        SearchProtocol.GetSuggestionsDatabaseResult(
          Seq(
            SearchProtocol.SuggestionsDatabaseUpdate.Add(1L, Suggestions.atom)
          ),
          1
        )
      )
    }

    "search entries by empty search query" in withDb { (repo, db) =>
      val handler = newSuggestionsHandler(repo, db)
      Await.ready(db.run(repo.insertAll(Suggestions.all)), Timeout)
      handler ! SearchProtocol.Completion(
        module     = "Test.Main",
        position   = Position(0, 0),
        selfType   = None,
        returnType = None,
        tags       = None
      )

      expectMsg(SearchProtocol.CompletionResult(Seq(), 4L))
    }

    "search entries by self type" in withDb { (repo, db) =>
      val handler = newSuggestionsHandler(repo, db)
      val Seq(_, methodId, _, _) =
        Await.result(db.run(repo.insertAll(Suggestions.all)), Timeout)
      handler ! SearchProtocol.Completion(
        module     = "Test.Main",
        position   = Position(0, 0),
        selfType   = Some("MyType"),
        returnType = None,
        tags       = None
      )

      expectMsg(SearchProtocol.CompletionResult(Seq(methodId).flatten, 4L))
    }

    "search entries by return type" in withDb { (repo, db) =>
      val handler = newSuggestionsHandler(repo, db)
      val Seq(_, _, functionId, _) =
        Await.result(db.run(repo.insertAll(Suggestions.all)), Timeout)
      handler ! SearchProtocol.Completion(
        module     = "Test.Main",
        position   = Position(0, 0),
        selfType   = None,
        returnType = Some("IO"),
        tags       = None
      )

      expectMsg(SearchProtocol.CompletionResult(Seq(functionId).flatten, 4L))
    }

    "search entries by tags" in withDb { (repo, db) =>
      val handler = newSuggestionsHandler(repo, db)
      val Seq(_, _, _, localId) =
        Await.result(db.run(repo.insertAll(Suggestions.all)), Timeout)
      handler ! SearchProtocol.Completion(
        module     = "Test.Main",
        position   = Position(0, 0),
        selfType   = None,
        returnType = None,
        tags       = Some(Seq(SearchProtocol.SuggestionKind.Local))
      )

      expectMsg(SearchProtocol.CompletionResult(Seq(localId).flatten, 4L))
    }

  }

  def newSuggestionsHandler(
    repo: SuggestionsRepo[DBIO],
    db: Database[DBIO, Future]
  ): ActorRef =
    system.actorOf(SuggestionsHandler.props(repo, db))

  def withDb(
    test: (SuggestionsRepo[DBIO], Database[DBIO, Future]) => Any
  ): Unit = {
    val dbPath = Files.createTempFile("suggestions", ".db")
    system.registerOnTermination(Files.deleteIfExists(dbPath))
    val repo = new SqlSuggestionsRepo()
    val db   = new SqlDatabase()
    Await.ready(db.run(repo.init), Timeout)

    try test(repo, db)
    finally db.close()
  }

}
