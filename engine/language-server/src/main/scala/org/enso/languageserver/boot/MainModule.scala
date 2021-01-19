package org.enso.languageserver.boot

import java.io.File
import java.net.URI

import akka.actor.ActorSystem
import org.enso.jsonrpc.JsonRpcServer
import org.enso.languageserver.boot.DeploymentType.{Azure, Desktop}
import org.enso.languageserver.capability.CapabilityRouter
import org.enso.languageserver.data._
import org.enso.languageserver.effect.ZioExec
import org.enso.languageserver.event.InitializedEvent
import org.enso.languageserver.filemanager.{
  FileManager,
  FileSystem,
  ReceivesTreeUpdatesHandler
}
import org.enso.languageserver.http.server.BinaryWebSocketServer
import org.enso.languageserver.io._
import org.enso.languageserver.monitoring.HealthCheckEndpoint
import org.enso.languageserver.protocol.binary.{
  BinaryConnectionControllerFactory,
  InboundMessageDecoder
}
import org.enso.languageserver.protocol.json.{
  JsonConnectionControllerFactory,
  JsonRpc
}
import org.enso.languageserver.requesthandler.monitoring.PingHandler
import org.enso.languageserver.runtime._
import org.enso.languageserver.search.SuggestionsHandler
import org.enso.languageserver.session.SessionRouter
import org.enso.languageserver.text.BufferRegistry
import org.enso.languageserver.util.binary.BinaryEncoder
import org.enso.loggingservice.{JavaLoggingLogHandler, LogLevel}
import org.enso.polyglot.{LanguageInfo, RuntimeOptions, RuntimeServerInfo}
import org.enso.searcher.sql.{SqlDatabase, SqlSuggestionsRepo, SqlVersionsRepo}
import org.enso.searcher.sqlite.LockingMode
import org.enso.text.{ContentBasedVersioning, Sha3_224VersionCalculator}
import org.graalvm.polyglot.Context
import org.graalvm.polyglot.io.MessageEndpoint
import org.slf4j.LoggerFactory

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}

/** A main module containing all components of the server.
  *
  * @param serverConfig configuration for the language server
  * @param logLevel log level for the Language Server
  */
class MainModule(serverConfig: LanguageServerConfig, logLevel: LogLevel) {

  val log = LoggerFactory.getLogger(this.getClass)
  log.trace("Initializing...")

  val directoriesConfig =
    DirectoriesConfig.initialize(serverConfig.contentRootPath)
  val languageServerConfig = Config(
    Map(serverConfig.contentRootUuid -> new File(serverConfig.contentRootPath)),
    FileManagerConfig(timeout = 3.seconds),
    PathWatcherConfig(),
    ExecutionContextConfig(),
    directoriesConfig
  )
  log.trace("Created Language Server config")

  val zioExec = ZioExec(zio.Runtime.default)
  log.trace("Created ZioExec")

  val fileSystem: FileSystem = new FileSystem
  log.trace("Created FileSystem")

  implicit val versionCalculator: ContentBasedVersioning =
    Sha3_224VersionCalculator
  log.trace("Created Version Calculator")

  implicit val system =
    ActorSystem(
      serverConfig.name,
      None,
      None,
      Some(serverConfig.computeExecutionContext)
    )
  log.trace("Created ActorSystem")

  val sqlDatabase =
    DeploymentType.fromEnvironment() match {
      case Desktop =>
        SqlDatabase(
          languageServerConfig.directories.suggestionsDatabaseFile.toString
        )

      case Azure =>
        SqlDatabase(
          languageServerConfig.directories.suggestionsDatabaseFile.toString,
          Some(LockingMode.UnixFlock)
        )
    }

  val suggestionsRepo = new SqlSuggestionsRepo(sqlDatabase)(system.dispatcher)
  val versionsRepo    = new SqlVersionsRepo(sqlDatabase)(system.dispatcher)
  log.trace("Created SQL Repos")

  lazy val sessionRouter =
    system.actorOf(SessionRouter.props(), "session-router")

  lazy val runtimeConnector =
    system.actorOf(RuntimeConnector.props, "runtime-connector")

  lazy val fileManager = system.actorOf(
    FileManager.pool(languageServerConfig, fileSystem, zioExec),
    "file-manager"
  )

  lazy val bufferRegistry =
    system.actorOf(
      BufferRegistry.props(versionsRepo, fileManager, runtimeConnector),
      "buffer-registry"
    )

  lazy val receivesTreeUpdatesHandler =
    system.actorOf(
      ReceivesTreeUpdatesHandler
        .props(languageServerConfig, fileSystem, zioExec),
      "file-event-registry"
    )

  lazy val suggestionsHandler =
    system.actorOf(
      SuggestionsHandler
        .props(
          languageServerConfig,
          suggestionsRepo,
          versionsRepo,
          sessionRouter,
          runtimeConnector
        ),
      "suggestions-handler"
    )

  lazy val capabilityRouter =
    system.actorOf(
      CapabilityRouter.props(
        bufferRegistry,
        receivesTreeUpdatesHandler,
        suggestionsHandler
      ),
      "capability-router"
    )

  lazy val contextRegistry =
    system.actorOf(
      ContextRegistry
        .props(
          suggestionsRepo,
          languageServerConfig,
          runtimeConnector,
          sessionRouter
        ),
      "context-registry"
    )

  val stdOut    = new ObservableOutputStream
  val stdErr    = new ObservableOutputStream
  val stdInSink = new ObservableOutputStream
  val stdIn     = new ObservablePipedInputStream(stdInSink)

  log.trace("Initializing Runtime context...")
  val context = Context
    .newBuilder(LanguageInfo.ID)
    .allowAllAccess(true)
    .allowExperimentalOptions(true)
    .option(RuntimeServerInfo.ENABLE_OPTION, "true")
    .option(RuntimeOptions.PACKAGES_PATH, serverConfig.contentRootPath)
    .option(
      RuntimeOptions.LOG_LEVEL,
      JavaLoggingLogHandler.getJavaLogLevelFor(logLevel).getName
    )
    .option(
      RuntimeServerInfo.JOB_PARALLELISM_OPTION,
      Runtime.getRuntime.availableProcessors().toString
    )
    .out(stdOut)
    .err(stdErr)
    .in(stdIn)
    .logHandler(
      JavaLoggingLogHandler.create(JavaLoggingLogHandler.defaultLevelMapping)
    )
    .serverTransport((uri: URI, peerEndpoint: MessageEndpoint) => {
      if (uri.toString == RuntimeServerInfo.URI) {
        val connection = new RuntimeConnector.Endpoint(
          runtimeConnector,
          peerEndpoint
        )
        runtimeConnector ! RuntimeConnector.Initialize(peerEndpoint)
        connection
      } else null
    })
    .build()
  context.initialize(LanguageInfo.ID)
  log.trace("Runtime context initialized")

  system.eventStream.setLogLevel(LogLevel.toAkka(logLevel))
  log.trace(s"Set akka log level to $logLevel")

  val runtimeKiller =
    system.actorOf(
      RuntimeKiller.props(runtimeConnector, context),
      "runtime-context"
    )

  val stdOutController =
    system.actorOf(
      OutputRedirectionController
        .props(stdOut, OutputKind.StandardOutput, sessionRouter),
      "std-out-controller"
    )

  val stdErrController =
    system.actorOf(
      OutputRedirectionController
        .props(stdErr, OutputKind.StandardError, sessionRouter),
      "std-err-controller"
    )

  val stdInController =
    system.actorOf(
      InputRedirectionController.props(stdIn, stdInSink, sessionRouter),
      "std-in-controller"
    )

  val jsonRpcControllerFactory = new JsonConnectionControllerFactory(
    bufferRegistry,
    capabilityRouter,
    fileManager,
    contextRegistry,
    suggestionsHandler,
    stdOutController,
    stdErrController,
    stdInController,
    runtimeConnector
  )
  log.trace("Created JsonConnectionControllerFactory")

  val pingHandlerProps =
    PingHandler.props(
      List(
        bufferRegistry,
        capabilityRouter,
        fileManager,
        contextRegistry
      ),
      10.seconds,
      true
    )

  val healthCheckEndpoint =
    new HealthCheckEndpoint(pingHandlerProps, system)(
      serverConfig.computeExecutionContext
    )

  val jsonRpcServer =
    new JsonRpcServer(
      JsonRpc.protocol,
      jsonRpcControllerFactory,
      JsonRpcServer
        .Config(outgoingBufferSize = 10000, lazyMessageTimeout = 10.seconds),
      List(healthCheckEndpoint)
    )
  log.trace("Created JsonRpcServer")

  val binaryServer =
    new BinaryWebSocketServer(
      InboundMessageDecoder,
      BinaryEncoder.empty,
      new BinaryConnectionControllerFactory(fileManager)
    )
  log.trace("Created BinaryWebSocketServer")

  /** Initialize the module. */
  def init: Future[Unit] = {
    import system.dispatcher

    val suggestionsRepoInit = suggestionsRepo.init
    suggestionsRepoInit.onComplete {
      case Success(()) =>
        system.eventStream.publish(InitializedEvent.SuggestionsRepoInitialized)
      case Failure(ex) =>
        log.error("Failed to initialize SQL suggestions repo", ex)
    }

    val versionsRepoInit = versionsRepo.init
    versionsRepoInit.onComplete {
      case Success(()) =>
        system.eventStream.publish(InitializedEvent.FileVersionsRepoInitialized)
      case Failure(ex) =>
        log.error("Failed to initialize SQL versions repo", ex)
    }(system.dispatcher)

    val initialization = Future
      .sequence(Seq(suggestionsRepoInit, versionsRepoInit))
      .map(_ => ())

    initialization.onComplete {
      case Success(()) =>
        system.eventStream.publish(InitializedEvent.InitializationFinished)
      case _ =>
        system.eventStream.publish(InitializedEvent.InitializationFailed)
    }

    initialization
  }

  /** Close the main module releasing all resources. */
  def close(): Unit = {
    suggestionsRepo.close()
    versionsRepo.close()
    log.trace("Closed MainModule")
  }
}
