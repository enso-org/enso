package org.enso.languageserver.search

import java.io.File
import java.nio.file.Files
import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.apache.commons.io.FileUtils
import org.enso.languageserver.capability.CapabilityProtocol.{
  AcquireCapability,
  CapabilityAcquired
}
import org.enso.languageserver.data._
import org.enso.languageserver.event.InitializedEvent
import org.enso.languageserver.filemanager.Path
import org.enso.languageserver.search.SearchProtocol.SuggestionDatabaseEntry
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.session.SessionRouter.DeliverToJsonController
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.searcher.{FileVersionsRepo, SuggestionsRepo}
import org.enso.searcher.sql.{SqlDatabase, SqlSuggestionsRepo, SqlVersionsRepo}
import org.enso.testkit.RetrySpec
import org.enso.text.editing.model.Position
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

class SuggestionsHandlerSpec
    extends TestKit(ActorSystem("TestSystem"))
    with ImplicitSender
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with RetrySpec {

  import system.dispatcher

  val Timeout: FiniteDuration = 10.seconds

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "SuggestionsHandler" should {

    "subscribe to notification updates" taggedAs Retry in withDb {
      (_, _, _, _, handler) =>
        val clientId = UUID.randomUUID()

        handler ! AcquireCapability(
          newJsonSession(clientId),
          CapabilityRegistration(ReceivesSuggestionsDatabaseUpdates())
        )
        expectMsg(CapabilityAcquired)
    }

    "receive runtime updates" taggedAs Retry in withDb {
      (_, repo, router, _, handler) =>
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
            SearchProtocol.SuggestionsDatabaseUpdateNotification(4L, updates)
          )
        )

        // check database entries exist
        val (_, records) = Await.result(repo.getAll, Timeout)
        records.map(
          _.suggestion
        ) should contain theSameElementsAs Suggestions.all
    }

    "apply runtime updates in correct order" taggedAs Retry in withDb {
      (_, repo, router, _, handler) =>
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
            SearchProtocol.SuggestionsDatabaseUpdateNotification(4L, updates)
          )
        )

        // check that database entries removed
        val (_, all) = Await.result(repo.getAll, Timeout)
        all.map(_.suggestion) should contain theSameElementsAs Suggestions.all
    }

    "get initial suggestions database version" taggedAs Retry in withDb {
      (_, _, _, _, handler) =>
        handler ! SearchProtocol.GetSuggestionsDatabaseVersion

        expectMsg(SearchProtocol.GetSuggestionsDatabaseVersionResult(0))
    }

    "get suggestions database version" taggedAs Retry in withDb {
      (_, repo, _, _, handler) =>
        Await.ready(repo.insert(Suggestions.atom), Timeout)
        handler ! SearchProtocol.GetSuggestionsDatabaseVersion

        expectMsg(SearchProtocol.GetSuggestionsDatabaseVersionResult(1))
    }

    "get initial suggestions database" taggedAs Retry in withDb {
      (_, _, _, _, handler) =>
        handler ! SearchProtocol.GetSuggestionsDatabase

        expectMsg(SearchProtocol.GetSuggestionsDatabaseResult(0, Seq()))
    }

    "get suggestions database" taggedAs Retry in withDb {
      (_, repo, _, _, handler) =>
        Await.ready(repo.insert(Suggestions.atom), Timeout)
        handler ! SearchProtocol.GetSuggestionsDatabase

        expectMsg(
          SearchProtocol.GetSuggestionsDatabaseResult(
            1,
            Seq(SuggestionDatabaseEntry(1L, Suggestions.atom))
          )
        )
    }

    "invalidate suggestions database" taggedAs Retry in withDb {
      (_, repo, _, connector, handler) =>
        Await.ready(repo.insert(Suggestions.atom), Timeout)
        handler ! SearchProtocol.InvalidateSuggestionsDatabase

        connector.expectMsgClass(classOf[Api.Request]) match {
          case Api.Request(_, Api.InvalidateModulesIndexRequest()) =>
          case Api.Request(_, msg) =>
            fail(s"Runtime connector receive unexpected message: $msg")
        }
        connector.reply(Api.Response(Api.InvalidateModulesIndexResponse()))

        expectMsg(SearchProtocol.InvalidateSuggestionsDatabaseResult)
    }

    "search entries by empty search query" taggedAs Retry in withDb {
      (config, repo, _, _, handler) =>
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

    "search entries by self type" taggedAs Retry in withDb {
      (config, repo, _, _, handler) =>
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

    "search entries by return type" taggedAs Retry in withDb {
      (config, repo, _, _, handler) =>
        val (_, Seq(_, _, functionId, _)) =
          Await.result(repo.insertAll(Suggestions.all), Timeout)
        handler ! SearchProtocol.Completion(
          file       = mkModulePath(config, "Main.enso"),
          position   = Position(1, 10),
          selfType   = None,
          returnType = Some("IO"),
          tags       = None
        )

        expectMsg(SearchProtocol.CompletionResult(4L, Seq(functionId).flatten))
    }

    "search entries by tags" taggedAs Retry in withDb {
      (config, repo, _, _, handler) =>
        val (_, Seq(_, _, _, localId)) =
          Await.result(repo.insertAll(Suggestions.all), Timeout)
        handler ! SearchProtocol.Completion(
          file       = mkModulePath(config, "Main.enso"),
          position   = Position(42, 0),
          selfType   = None,
          returnType = None,
          tags       = Some(Seq(SearchProtocol.SuggestionKind.Local))
        )

        expectMsg(SearchProtocol.CompletionResult(4L, Seq(localId).flatten))
    }
  }

  def newSuggestionsHandler(
    config: Config,
    sessionRouter: TestProbe,
    runtimeConnector: TestProbe,
    suggestionsRepo: SuggestionsRepo[Future],
    fileVersionsRepo: FileVersionsRepo[Future]
  )(implicit versionCalculator: ContentBasedVersioning): ActorRef = {
    val handler =
      system.actorOf(
        SuggestionsHandler.props(
          config,
          suggestionsRepo,
          fileVersionsRepo,
          sessionRouter.ref,
          runtimeConnector.ref
        )
      )
    handler ! SuggestionsHandler.ProjectNameUpdated("Test")
    handler
  }

  def newConfig(root: File): Config = {
    Config(
      Map(UUID.randomUUID() -> root),
      FileManagerConfig(timeout = 3.seconds),
      PathWatcherConfig(),
      ExecutionContextConfig(requestTimeout = 3.seconds),
      DirectoriesConfig(root)
    )
  }

  def mkModulePath(config: Config, segments: String*): Path = {
    val (rootId, _) = config.contentRoots.head
    Path(rootId, "src" +: segments.toVector)
  }

  def newJsonSession(clientId: UUID): JsonSession =
    JsonSession(clientId, TestProbe().ref)

  def withDb(
    test: (
      Config,
      SuggestionsRepo[Future],
      TestProbe,
      TestProbe,
      ActorRef
    ) => Any
  ): Unit = {
    implicit val versionCalc = Sha3_224VersionCalculator
    val testContentRoot      = Files.createTempDirectory(null).toRealPath()
    sys.addShutdownHook(FileUtils.deleteQuietly(testContentRoot.toFile))
    val config          = newConfig(testContentRoot.toFile)
    val router          = TestProbe("session-router")
    val connector       = TestProbe("runtime-connector")
    val sqlDatabase     = SqlDatabase(config.directories.suggestionsDatabaseFile)
    val suggestionsRepo = new SqlSuggestionsRepo(sqlDatabase)
    val versionsRepo    = new SqlVersionsRepo(sqlDatabase)
    val handler = newSuggestionsHandler(
      config,
      router,
      connector,
      suggestionsRepo,
      versionsRepo
    )
    suggestionsRepo.init.onComplete {
      case Success(()) =>
        system.eventStream.publish(InitializedEvent.SuggestionsRepoInitialized)
      case Failure(ex) =>
        system.log.error(ex, "Failed to initialize Suggestions repo")
    }
    versionsRepo.init.onComplete {
      case Success(()) =>
        system.eventStream.publish(InitializedEvent.FileVersionsRepoInitialized)
      case Failure(ex) =>
        system.log.error(ex, "Failed to initialize FileVersions repo")
    }

    try test(config, suggestionsRepo, router, connector, handler)
    finally {
      system.stop(handler)
      sqlDatabase.close()
    }
  }

}
