package org.enso.languageserver.runtime

import java.nio.file.Files
import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit}
import org.enso.jsonrpc.test.RetrySpec
import org.enso.languageserver.data.{
  Config,
  ExecutionContextConfig,
  FileManagerConfig,
  PathWatcherConfig
}
import org.enso.languageserver.filemanager.Path
import org.enso.languageserver.refactoring.ProjectNameChangedEvent
import org.enso.searcher.SuggestionsRepo
import org.enso.searcher.sql.SqlSuggestionsRepo
import org.enso.text.editing.model.Position
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

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

    "get initial suggestions database version" taggedAs Retry() in withDb {
      repo =>
        val (handler, _) = newSuggestionsHandler(repo)
        handler ! SearchProtocol.GetSuggestionsDatabaseVersion

        expectMsg(SearchProtocol.GetSuggestionsDatabaseVersionResult(0))
    }

    "get suggestions database version" taggedAs Retry() in withDb { repo =>
      val (handler, _) = newSuggestionsHandler(repo)
      Await.ready(repo.insert(Suggestions.atom), Timeout)

      handler ! SearchProtocol.GetSuggestionsDatabaseVersion

      expectMsg(SearchProtocol.GetSuggestionsDatabaseVersionResult(1))
    }

    "get initial suggestions database" taggedAs Retry() in withDb { repo =>
      val (handler, _) = newSuggestionsHandler(repo)
      handler ! SearchProtocol.GetSuggestionsDatabase

      expectMsg(SearchProtocol.GetSuggestionsDatabaseResult(Seq(), 0))
    }

    "get suggestions database" taggedAs Retry() in withDb { repo =>
      val (handler, _) = newSuggestionsHandler(repo)
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
      val (handler, config) = newSuggestionsHandler(repo)
      Await.ready(repo.insertAll(Suggestions.all), Timeout)
      handler ! SearchProtocol.Completion(
        file       = mkModulePath(config, "Foo", "Main.enso"),
        position   = Position(0, 0),
        selfType   = None,
        returnType = None,
        tags       = None
      )

      expectMsg(SearchProtocol.CompletionResult(4L, Seq()))
    }

    "search entries by self type" taggedAs Retry() in withDb { repo =>
      val (handler, config) = newSuggestionsHandler(repo)
      val (_, Seq(_, methodId, _, _)) =
        Await.result(repo.insertAll(Suggestions.all), Timeout)
      handler ! SearchProtocol.Completion(
        file       = mkModulePath(config, "Main.enso"),
        position   = Position(0, 0),
        selfType   = Some("MyType"),
        returnType = None,
        tags       = None
      )

      expectMsg(SearchProtocol.CompletionResult(4L, Seq(methodId).flatten))
    }

    "search entries by return type" taggedAs Retry() in withDb { repo =>
      val (handler, config) = newSuggestionsHandler(repo)
      val (_, Seq(_, _, functionId, _)) =
        Await.result(repo.insertAll(Suggestions.all), Timeout)
      handler ! SearchProtocol.Completion(
        file       = mkModulePath(config, "Main.enso"),
        position   = Position(0, 0),
        selfType   = None,
        returnType = Some("IO"),
        tags       = None
      )

      expectMsg(SearchProtocol.CompletionResult(4L, Seq(functionId).flatten))
    }

    "search entries by tags" taggedAs Retry() in withDb { repo =>
      val (handler, config) = newSuggestionsHandler(repo)
      val (_, Seq(_, _, _, localId)) =
        Await.result(repo.insertAll(Suggestions.all), Timeout)
      handler ! SearchProtocol.Completion(
        file       = mkModulePath(config, "Main.enso"),
        position   = Position(0, 0),
        selfType   = None,
        returnType = None,
        tags       = Some(Seq(SearchProtocol.SuggestionKind.Local))
      )

      expectMsg(SearchProtocol.CompletionResult(4L, Seq(localId).flatten))
    }

  }

  def newSuggestionsHandler(
    repo: SuggestionsRepo[Future]
  ): (ActorRef, Config) = {
    val testContentRoot = Files.createTempDirectory(null).toRealPath()
    testContentRoot.toFile.deleteOnExit()
    val testContentRootId = UUID.randomUUID()
    val config = Config(
      Map(testContentRootId -> testContentRoot.toFile),
      FileManagerConfig(timeout = 3.seconds),
      PathWatcherConfig(),
      ExecutionContextConfig(requestTimeout = 3.seconds)
    )
    val handler = system.actorOf(SuggestionsHandler.props(config, repo))
    handler ! ProjectNameChangedEvent("Test")
    (handler, config)
  }

  def mkModulePath(config: Config, segments: String*): Path = {
    val (rootId, _) = config.contentRoots.head
    Path(rootId, "src" +: segments.toVector)
  }

  def withDb(test: SuggestionsRepo[Future] => Any): Unit = {
    val dbPath = Files.createTempFile("suggestions", ".db")
    system.registerOnTermination(Files.deleteIfExists(dbPath))
    val repo = SqlSuggestionsRepo()
    Await.ready(repo.init, Timeout)

    try test(repo)
    finally repo.close()
  }

}
