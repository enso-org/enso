package org.enso.languageserver.runtime

import java.nio.file.Files

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit}
import org.enso.searcher.SuggestionsRepo
import org.enso.searcher.sql.SqlSuggestionsRepo
import org.enso.text.editing.model.Position
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike
import org.enso.testkit.RetrySpec

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class SuggestionsHandlerTest
    extends TestKit(ActorSystem("TestSystem"))
    with ImplicitSender
    with AnyWordSpecLike
    with BeforeAndAfterAll
    with RetrySpec {

  import system.dispatcher

  val Timeout: FiniteDuration = 10.seconds

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "SuggestionsHandler" should {

    "get initial suggestions database version" in withDb { repo =>
      val handler = newSuggestionsHandler(repo)
      handler ! SearchProtocol.GetSuggestionsDatabaseVersion

      expectMsg(SearchProtocol.GetSuggestionsDatabaseVersionResult(0))
    }

    "get suggestions database version" in withDb { repo =>
      val handler = newSuggestionsHandler(repo)
      Await.ready(repo.insert(Suggestions.atom), Timeout)

      handler ! SearchProtocol.GetSuggestionsDatabaseVersion

      expectMsg(SearchProtocol.GetSuggestionsDatabaseVersionResult(1))
    }

    "get initial suggestions database" in withDb { repo =>
      val handler = newSuggestionsHandler(repo)
      handler ! SearchProtocol.GetSuggestionsDatabase

      expectMsg(SearchProtocol.GetSuggestionsDatabaseResult(Seq(), 0))
    }

    "get suggestions database" in withDb { repo =>
      val handler = newSuggestionsHandler(repo)
      Await.ready(repo.insert(Suggestions.atom), Timeout)
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

    "search entries by empty search query" taggedAs Retry() in withDb { repo =>
      val handler = newSuggestionsHandler(repo)
      Await.ready(repo.insertAll(Suggestions.all), Timeout)
      handler ! SearchProtocol.Completion(
        module     = "Test.Main",
        position   = Position(0, 0),
        selfType   = None,
        returnType = None,
        tags       = None
      )

      expectMsg(SearchProtocol.CompletionResult(4L, Seq()))
    }

    "search entries by self type" taggedAs Retry() in withDb { repo =>
      val handler = newSuggestionsHandler(repo)
      val (_, Seq(_, methodId, _, _)) =
        Await.result(repo.insertAll(Suggestions.all), Timeout)
      handler ! SearchProtocol.Completion(
        module     = "Test.Main",
        position   = Position(0, 0),
        selfType   = Some("MyType"),
        returnType = None,
        tags       = None
      )

      expectMsg(SearchProtocol.CompletionResult(4L, Seq(methodId).flatten))
    }

    "search entries by return type" taggedAs Retry() in withDb { repo =>
      val handler = newSuggestionsHandler(repo)
      val (_, Seq(_, _, functionId, _)) =
        Await.result(repo.insertAll(Suggestions.all), Timeout)
      handler ! SearchProtocol.Completion(
        module     = "Test.Main",
        position   = Position(0, 0),
        selfType   = None,
        returnType = Some("IO"),
        tags       = None
      )

      expectMsg(SearchProtocol.CompletionResult(4L, Seq(functionId).flatten))
    }

    "search entries by tags" taggedAs Retry() in withDb { repo =>
      val handler = newSuggestionsHandler(repo)
      val (_, Seq(_, _, _, localId)) =
        Await.result(repo.insertAll(Suggestions.all), Timeout)
      handler ! SearchProtocol.Completion(
        module     = "Test.Main",
        position   = Position(0, 0),
        selfType   = None,
        returnType = None,
        tags       = Some(Seq(SearchProtocol.SuggestionKind.Local))
      )

      expectMsg(SearchProtocol.CompletionResult(4L, Seq(localId).flatten))
    }

  }

  def newSuggestionsHandler(repo: SuggestionsRepo[Future]): ActorRef =
    system.actorOf(SuggestionsHandler.props(repo))

  def withDb(test: SuggestionsRepo[Future] => Any): Unit = {
    val dbPath = Files.createTempFile("suggestions", ".db")
    system.registerOnTermination(Files.deleteIfExists(dbPath))
    val repo = SqlSuggestionsRepo()
    Await.ready(repo.init, Timeout)

    try test(repo)
    finally repo.close()
  }

}
