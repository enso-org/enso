package org.enso.languageserver.runtime

import java.io.File
import java.nio.file.Files
import java.util.UUID
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.apache.commons.io.FileUtils
import org.enso.languageserver.data.{
  Config,
  DirectoriesConfig,
  ExecutionContextConfig,
  FileManagerConfig,
  PathWatcherConfig
}
import org.enso.languageserver.event.InitializedEvent
import org.enso.languageserver.runtime.ContextRegistryProtocol.{
  ExecutionDiagnostic,
  ExecutionDiagnosticKind,
  ExecutionDiagnosticNotification,
  ExecutionFailedNotification,
  ExecutionFailure,
  ExpressionUpdatesNotification,
  VisualisationContext,
  VisualisationEvaluationFailed,
  VisualisationUpdate
}
import org.enso.languageserver.search.Suggestions
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.session.SessionRouter.{
  DeliverToBinaryController,
  DeliverToJsonController
}
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.searcher.SuggestionsRepo
import org.enso.searcher.sql.SqlSuggestionsRepo
import org.enso.testkit.RetrySpec
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class ContextEventsListenerSpec
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

  "ContextEventsListener" should {

    "not send empty updates" taggedAs Retry in withDb { (_, _, _, router, _) =>
      router.expectNoMessage()
    }

    "send expression updates" taggedAs Retry in withDb {
      (clientId, contextId, repo, router, listener) =>
        val (_, suggestionIds) = Await.result(
          repo.insertAll(
            Seq(
              Suggestions.atom,
              Suggestions.method,
              Suggestions.function,
              Suggestions.local
            )
          ),
          Timeout
        )

        listener ! Api.ExpressionUpdates(
          contextId,
          Set(
            Api.ExpressionUpdate(
              Suggestions.method.externalId.get,
              Some(Suggestions.method.returnType),
              Some(
                Api.MethodPointer(
                  Suggestions.method.module,
                  Suggestions.method.selfType,
                  Suggestions.method.name
                )
              ),
              Vector(),
              false,
              Api.ExpressionUpdate.Payload.Value()
            )
          )
        )

        router.expectMsg(
          DeliverToJsonController(
            clientId,
            ContextRegistryProtocol.ExpressionUpdatesNotification(
              contextId,
              Vector(
                ContextRegistryProtocol.ExpressionUpdate(
                  Suggestions.method.externalId.get,
                  Some(Suggestions.method.returnType),
                  Some(suggestionIds(1).get),
                  Vector(),
                  false,
                  ContextRegistryProtocol.ExpressionUpdate.Payload.Value
                )
              )
            )
          )
        )
    }

    "send dataflow error updates" taggedAs Retry in withDb {
      (clientId, contextId, _, router, listener) =>
        listener ! Api.ExpressionUpdates(
          contextId,
          Set(
            Api.ExpressionUpdate(
              Suggestions.method.externalId.get,
              None,
              None,
              Vector(),
              false,
              Api.ExpressionUpdate.Payload.DataflowError(
                Seq(Suggestions.function.externalId.get)
              )
            )
          )
        )

        router.expectMsg(
          DeliverToJsonController(
            clientId,
            ContextRegistryProtocol.ExpressionUpdatesNotification(
              contextId,
              Vector(
                ContextRegistryProtocol.ExpressionUpdate(
                  Suggestions.method.externalId.get,
                  None,
                  None,
                  Vector(),
                  false,
                  ContextRegistryProtocol.ExpressionUpdate.Payload
                    .DataflowError(Seq(Suggestions.function.externalId.get))
                )
              )
            )
          )
        )
    }

    "send runtime error updates" taggedAs Retry in withDb {
      (clientId, contextId, _, router, listener) =>
        listener ! Api.ExpressionUpdates(
          contextId,
          Set(
            Api.ExpressionUpdate(
              Suggestions.method.externalId.get,
              None,
              None,
              Vector(),
              false,
              Api.ExpressionUpdate.Payload.Panic("Method failure", Seq())
            )
          )
        )

        router.expectMsg(
          DeliverToJsonController(
            clientId,
            ContextRegistryProtocol.ExpressionUpdatesNotification(
              contextId,
              Vector(
                ContextRegistryProtocol.ExpressionUpdate(
                  Suggestions.method.externalId.get,
                  None,
                  None,
                  Vector(),
                  false,
                  ContextRegistryProtocol.ExpressionUpdate.Payload
                    .Panic("Method failure", Seq())
                )
              )
            )
          )
        )
    }

    "send expression updates grouped" taggedAs Retry in withDb(0.seconds) {
      (clientId, contextId, repo, router, listener) =>
        Await.result(
          repo.insertAll(
            Seq(
              Suggestions.atom,
              Suggestions.method,
              Suggestions.function,
              Suggestions.local
            )
          ),
          Timeout
        )

        listener ! Api.ExpressionUpdates(
          contextId,
          Set(
            Api.ExpressionUpdate(
              Suggestions.method.externalId.get,
              None,
              None,
              Vector(),
              false,
              Api.ExpressionUpdate.Payload.Value()
            )
          )
        )

        listener ! Api.ExpressionUpdates(
          contextId,
          Set(
            Api.ExpressionUpdate(
              Suggestions.local.externalId.get,
              None,
              None,
              Vector(),
              false,
              Api.ExpressionUpdate.Payload.Value()
            )
          )
        )

        listener ! ContextEventsListener.RunExpressionUpdates

        router.expectMsg(
          DeliverToJsonController(
            clientId,
            ExpressionUpdatesNotification(
              contextId,
              Vector(
                ContextRegistryProtocol.ExpressionUpdate(
                  Suggestions.method.externalId.get,
                  None,
                  None,
                  Vector(),
                  false,
                  ContextRegistryProtocol.ExpressionUpdate.Payload.Value
                ),
                ContextRegistryProtocol.ExpressionUpdate(
                  Suggestions.local.externalId.get,
                  None,
                  None,
                  Vector(),
                  false,
                  ContextRegistryProtocol.ExpressionUpdate.Payload.Value
                )
              )
            )
          )
        )
    }

    "send visualization updates" taggedAs Retry in withDb {
      (clientId, contextId, _, router, listener) =>
        val ctx = Api.VisualisationContext(
          UUID.randomUUID(),
          contextId,
          UUID.randomUUID()
        )
        val data = Array[Byte](1, 2, 3)
        listener ! Api.VisualisationUpdate(ctx, data)

        router.expectMsg(
          DeliverToBinaryController(
            clientId,
            VisualisationUpdate(
              VisualisationContext(
                ctx.visualisationId,
                ctx.contextId,
                ctx.expressionId
              ),
              data
            )
          )
        )
    }

    "send execution failed notification" taggedAs Retry in withDb {
      (clientId, contextId, _, router, listener) =>
        val message = "Test execution failed"
        listener ! Api.ExecutionFailed(
          contextId,
          Api.ExecutionResult.Failure(message, None)
        )

        router.expectMsg(
          DeliverToJsonController(
            clientId,
            ExecutionFailedNotification(
              contextId,
              ExecutionFailure(
                message,
                None
              )
            )
          )
        )
    }

    "send execution update notification" taggedAs Retry in withDb {
      (clientId, contextId, _, router, listener) =>
        val message = "Test execution failed"
        listener ! Api.ExecutionUpdate(
          contextId,
          Seq(Api.ExecutionResult.Diagnostic.error(message))
        )

        router.expectMsg(
          DeliverToJsonController(
            clientId,
            ExecutionDiagnosticNotification(
              contextId,
              Seq(
                ExecutionDiagnostic(
                  ExecutionDiagnosticKind.Error,
                  message,
                  None,
                  None,
                  None,
                  Vector()
                )
              )
            )
          )
        )
    }

    "send visualisation evaluation failed notification" taggedAs Retry in withDb {
      (clientId, contextId, _, router, listener) =>
        val message         = "Test visualisation evaluation failed"
        val visualisationId = UUID.randomUUID()
        val expressionId    = UUID.randomUUID()
        listener ! Api.VisualisationEvaluationFailed(
          contextId,
          visualisationId,
          expressionId,
          message,
          None
        )

        router.expectMsg(
          DeliverToJsonController(
            clientId,
            VisualisationEvaluationFailed(
              contextId,
              visualisationId,
              expressionId,
              message,
              None
            )
          )
        )
    }
  }

  def newConfig(root: File): Config = {
    Config(
      Map(UUID.randomUUID() -> root),
      FileManagerConfig(timeout = 3.seconds),
      PathWatcherConfig(),
      ExecutionContextConfig(requestTimeout = 3.seconds),
      DirectoriesConfig.initialize(root)
    )
  }

  def newJsonSession(clientId: UUID): JsonSession =
    JsonSession(clientId, TestProbe().ref)

  def withDb(
    test: (UUID, UUID, SuggestionsRepo[Future], TestProbe, ActorRef) => Any
  ): Unit =
    withDb(100.millis)(test)

  def withDb(updatesSendRate: FiniteDuration)(
    test: (UUID, UUID, SuggestionsRepo[Future], TestProbe, ActorRef) => Any
  ): Unit = {
    val testContentRoot = Files.createTempDirectory(null).toRealPath()
    sys.addShutdownHook(FileUtils.deleteQuietly(testContentRoot.toFile))
    val config    = newConfig(testContentRoot.toFile)
    val clientId  = UUID.randomUUID()
    val contextId = UUID.randomUUID()
    val router    = TestProbe("session-router")
    val repo      = SqlSuggestionsRepo(config.directories.suggestionsDatabaseFile)
    val listener = system.actorOf(
      ContextEventsListener.props(
        config,
        repo,
        newJsonSession(clientId),
        contextId,
        router.ref,
        updatesSendRate
      )
    )
    repo.init.onComplete {
      case Success(()) =>
        system.eventStream.publish(InitializedEvent.SuggestionsRepoInitialized)
      case Failure(ex) =>
        system.log.error(ex, "Failed to initialize Suggestions repo")
    }

    try test(clientId, contextId, repo, router, listener)
    finally {
      system.stop(listener)
      repo.close()
    }
  }

}
