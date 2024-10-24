package org.enso.languageserver.boot

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.typesafe.config.ConfigFactory
import org.enso.distribution.locking.{
  ResourceManager,
  ThreadSafeFileLockManager
}
import org.enso.distribution.{DistributionManager, Environment, LanguageHome}
import org.enso.editions.EditionResolver
import org.enso.editions.updater.EditionManager
import org.enso.filewatcher.WatcherAdapterFactory
import org.enso.jsonrpc.{JsonRpcServer, SecureConnectionConfig}
import org.enso.runner.common.CompilerBasedDependencyExtractor
import org.enso.languageserver.capability.CapabilityRouter
import org.enso.languageserver.data._
import org.enso.languageserver.effect
import org.enso.languageserver.filemanager._
import org.enso.languageserver.http.server.BinaryWebSocketServer
import org.enso.languageserver.io._
import org.enso.languageserver.libraries._
import org.enso.languageserver.monitoring.{
  HealthCheckEndpoint,
  IdlenessEndpoint,
  IdlenessMonitor
}
import org.enso.languageserver.profiling.{EventsMonitorActor, ProfilingManager}
import org.enso.languageserver.protocol.binary.{
  BinaryConnectionControllerFactory,
  InboundMessageDecoder
}
import org.enso.languageserver.protocol.json.{
  JsonConnectionControllerFactory,
  JsonRpcProtocolFactory
}
import org.enso.languageserver.requesthandler.monitoring.PingHandler
import org.enso.languageserver.runtime._
import org.enso.languageserver.runtime.events.RuntimeEventsMonitor
import org.enso.languageserver.search.SuggestionsHandler
import org.enso.languageserver.session.SessionRouter
import org.enso.languageserver.text.BufferRegistry
import org.enso.languageserver.util.binary.BinaryEncoder
import org.enso.languageserver.vcsmanager.{Git, VcsManager}
import org.enso.librarymanager.LibraryLocations
import org.enso.librarymanager.local.DefaultLocalLibraryProvider
import org.enso.librarymanager.published.PublishedLibraryCache
import org.enso.lockmanager.server.LockManagerService
import org.enso.logger.masking.Masking
import org.enso.common.RuntimeOptions
import org.enso.common.ContextFactory
import org.enso.logging.utils.akka.AkkaConverter
import org.enso.polyglot.RuntimeServerInfo
import org.enso.profiling.events.NoopEventsMonitor
import org.enso.searcher.memory.InMemorySuggestionsRepo
import org.enso.text.{ContentBasedVersioning, Sha3_224VersionCalculator}
import org.enso.version.BuildVersion
import org.graalvm.polyglot.io.MessageEndpoint
import org.slf4j.event.Level
import org.slf4j.LoggerFactory

import java.io.{File, PrintStream}
import java.net.URI
import java.nio.charset.StandardCharsets
import java.time.Clock
import scala.concurrent.duration.DurationInt

/** A main module containing all components of the server.
  *
  * @param serverConfig configuration for the language server
  * @param logLevel log level for the Language Server
  */
class MainModule(serverConfig: LanguageServerConfig, logLevel: Level) {

  private val log = LoggerFactory.getLogger(this.getClass)
  log.debug(
    "Initializing main module of the Language Server from [{}, {}, {}]",
    BuildVersion.currentEdition,
    serverConfig,
    logLevel
  )

  private val ydocSupervisor    = new ComponentSupervisor()
  private val contextSupervisor = new ComponentSupervisor()
  private val utcClock          = Clock.systemUTC()

  val directoriesConfig = ProjectDirectoriesConfig(serverConfig.contentRootPath)
  private val contentRoot = ContentRootWithFile(
    ContentRoot.Project(serverConfig.contentRootUuid),
    new File(serverConfig.contentRootPath)
  )

  private val openAiKey = Option(java.lang.System.getenv("OPENAI_API_KEY"))
  private val openAiCfg = openAiKey.map(AICompletionConfig)

  val languageServerConfig = Config(
    contentRoot,
    FileManagerConfig(timeout = 3.seconds),
    VcsManagerConfig(
      initTimeout = 5.seconds,
      timeout     = 3.seconds,
      asyncInit   = true
    ),
    PathWatcherConfig(),
    ExecutionContextConfig(),
    directoriesConfig,
    serverConfig.profilingConfig,
    serverConfig.startupConfig,
    openAiCfg
  )
  log.trace("Created Language Server config [{}]", languageServerConfig)

  val system: ActorSystem =
    ActorSystem(
      serverConfig.name,
      None,
      Some(getClass.getClassLoader),
      Some(serverConfig.computeExecutionContext)
    )
  log.trace("Created ActorSystem [{}]", system)

  private val zioRuntime =
    new effect.ExecutionContextRuntime(system.dispatcher)
  private val zioExec = effect.ZioExec(zioRuntime)
  log.trace("Created ZIO executor [{}]", zioExec)

  private val fileSystem: FileSystem = new FileSystem(log)
  log.trace("Created file system [{}]", fileSystem)

  val git = Git.withEmptyUserConfig(
    Some(languageServerConfig.vcsManager.dataDirectory),
    languageServerConfig.vcsManager.asyncInit
  )
  log.trace("Created git [{}]", git)

  implicit val versionCalculator: ContentBasedVersioning =
    Sha3_224VersionCalculator
  log.trace("Created Version Calculator [{}]", versionCalculator)

  val suggestionsRepo =
    new InMemorySuggestionsRepo()(
      system.dispatcher
    );
  log.trace("Created SQL suggestions repo: [{}]", suggestionsRepo)

  val idlenessMonitor =
    system.actorOf(IdlenessMonitor.props(utcClock))

  lazy val sessionRouter =
    system.actorOf(SessionRouter.props(), "session-router")

  val environment         = new Environment {}
  val languageHome        = LanguageHome.detectFromExecutableLocation(environment)
  val distributionManager = new DistributionManager(environment)

  val editionProvider =
    EditionManager.makeEditionProvider(
      distributionManager,
      Some(languageHome),
      false
    )
  val editionResolver = EditionResolver(editionProvider)
  val editionReferenceResolver = new EditionReferenceResolver(
    contentRoot.file,
    editionProvider,
    editionResolver
  )
  val editionManager = EditionManager(distributionManager, Some(languageHome))
  val lockManager = new ThreadSafeFileLockManager(
    distributionManager.paths.locks
  )
  val resourceManager = new ResourceManager(lockManager)

  val lockManagerService = system.actorOf(
    LockManagerService.props(lockManager),
    "lock-manager-service"
  )

  private val (runtimeEventsMonitor, messagesCallbackOpt) =
    languageServerConfig.profiling.profilingEventsLogPath match {
      case Some(path) =>
        val out = new PrintStream(path.toFile, StandardCharsets.UTF_8)
        new RuntimeEventsMonitor(out) -> Some(())
      case None =>
        new NoopEventsMonitor() -> None
    }
  log.trace(
    "Started runtime events monitor [{}]",
    runtimeEventsMonitor.getClass.getName
  )

  private val eventsMonitor =
    system.actorOf(
      EventsMonitorActor.props(runtimeEventsMonitor),
      "events-monitor"
    )

  private val messagesCallback =
    messagesCallbackOpt
      .map(_ => EventsMonitorActor.messagesCallback(eventsMonitor))
      .toList

  private val profilingManager =
    system.actorOf(
      ProfilingManager.props(eventsMonitor, distributionManager),
      "profiling-manager"
    )

  lazy val runtimeConnector =
    system.actorOf(
      RuntimeConnector.props(lockManagerService, eventsMonitor),
      "runtime-connector"
    )

  lazy val contentRootManagerActor =
    system.actorOf(
      ContentRootManagerActor.props(languageServerConfig),
      "content-root-manager"
    )

  lazy val contentRootManagerWrapper: ContentRootManager =
    new ContentRootManagerWrapper(languageServerConfig, contentRootManagerActor)

  lazy val fileManager = system.actorOf(
    FileManager.props(
      languageServerConfig.fileManager,
      contentRootManagerWrapper,
      fileSystem,
      zioExec
    ),
    "file-manager"
  )

  lazy val vcsManager = system.actorOf(
    VcsManager.props(
      languageServerConfig.vcsManager,
      git,
      contentRootManagerWrapper,
      zioExec
    ),
    "vcs-manager"
  )

  lazy val bufferRegistry =
    system.actorOf(
      BufferRegistry.props(
        fileManager,
        vcsManager,
        runtimeConnector,
        contentRootManagerWrapper,
        TimingsConfig.default().withAutoSave(6.seconds)
      ),
      "buffer-registry"
    )

  lazy val receivesTreeUpdatesHandler =
    system.actorOf(
      ReceivesTreeUpdatesHandler.props(
        languageServerConfig,
        contentRootManagerWrapper,
        new WatcherAdapterFactory,
        fileSystem,
        zioExec
      ),
      "file-event-registry"
    )

  lazy val suggestionsHandler =
    system.actorOf(
      SuggestionsHandler
        .props(
          languageServerConfig,
          contentRootManagerWrapper,
          suggestionsRepo,
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
          languageServerConfig,
          RuntimeFailureMapper(contentRootManagerWrapper),
          runtimeConnector,
          sessionRouter
        ),
      "context-registry"
    )

  val stdOut    = new ObservableOutputStream
  val stdErr    = new ObservableOutputStream
  val stdInSink = new ObservableOutputStream
  val stdIn     = new ObservablePipedInputStream(stdInSink)

  val extraOptions = new java.util.HashMap[String, String]()
  extraOptions.put(RuntimeServerInfo.ENABLE_OPTION, "true")
  extraOptions.put(RuntimeOptions.INTERACTIVE_MODE, "true")
  extraOptions.put(
    RuntimeOptions.LOG_MASKING,
    Masking.isMaskingEnabled.toString
  )
  extraOptions.put(RuntimeOptions.EDITION_OVERRIDE, BuildVersion.currentEdition)
  extraOptions.put(
    RuntimeOptions.JOB_PARALLELISM,
    Runtime.getRuntime.availableProcessors().toString
  )

  val builder = ContextFactory
    .create()
    .projectRoot(serverConfig.contentRootPath)
    .logLevel(logLevel)
    .strictErrors(false)
    .disableLinting(false)
    .enableIrCaches(true)
    .out(stdOut)
    .err(stdErr)
    .in(stdIn)
    .options(extraOptions)
    .messageTransport((uri: URI, peerEndpoint: MessageEndpoint) => {
      if (uri.toString == RuntimeServerInfo.URI) {
        val connection = new RuntimeConnector.Endpoint(
          runtimeConnector,
          peerEndpoint
        )
        runtimeConnector ! RuntimeConnector.Initialize(peerEndpoint)
        connection
      } else null
    })
  if (System.getProperty("enso.dev.insight") != null) {
    stdOut.attach(arr => System.out.write(arr))
  }

  system.eventStream.setLogLevel(AkkaConverter.toAkka(logLevel))
  log.trace("Set akka log level to [{}]", logLevel)

  val runtimeKiller =
    system.actorOf(
      RuntimeKiller.props(runtimeConnector, contextSupervisor),
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

  val projectSettingsManager = system.actorOf(
    ProjectSettingsManager.props(contentRoot.file, editionResolver),
    "project-settings-manager"
  )

  val libraryLocations =
    LibraryLocations.resolve(
      distributionManager,
      Some(languageHome),
      Some(contentRoot.file.toPath)
    )

  val localLibraryManager = system.actorOf(
    LocalLibraryManager.props(contentRoot.file, libraryLocations),
    "local-library-manager"
  )

  val libraryConfig = LibraryConfig(
    localLibraryManager      = localLibraryManager,
    editionReferenceResolver = editionReferenceResolver,
    editionManager           = editionManager,
    localLibraryProvider     = DefaultLocalLibraryProvider.make(libraryLocations),
    publishedLibraryCache =
      PublishedLibraryCache.makeReadOnlyCache(libraryLocations),
    installerConfig = LibraryInstallerConfig(
      distributionManager,
      resourceManager,
      Some(languageHome),
      new CompilerBasedDependencyExtractor(logLevel)
    )
  )

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

  private val healthCheckEndpoint =
    new HealthCheckEndpoint(pingHandlerProps, system)(
      serverConfig.computeExecutionContext
    )

  private val idlenessEndpoint =
    new IdlenessEndpoint(idlenessMonitor)

  private val jsonRpcProtocolFactory = new JsonRpcProtocolFactory

  private val initializationComponent =
    ResourcesInitialization(
      system.eventStream,
      directoriesConfig,
      jsonRpcProtocolFactory,
      suggestionsRepo,
      builder,
      contextSupervisor,
      zioRuntime,
      ydocSupervisor
    )(system.dispatcher)

  private val jsonRpcControllerFactory = new JsonConnectionControllerFactory(
    mainComponent          = initializationComponent,
    bufferRegistry         = bufferRegistry,
    capabilityRouter       = capabilityRouter,
    fileManager            = fileManager,
    vcsManager             = vcsManager,
    contentRootManager     = contentRootManagerActor,
    contextRegistry        = contextRegistry,
    suggestionsHandler     = suggestionsHandler,
    stdOutController       = stdOutController,
    stdErrController       = stdErrController,
    stdInController        = stdInController,
    runtimeConnector       = runtimeConnector,
    idlenessMonitor        = idlenessMonitor,
    projectSettingsManager = projectSettingsManager,
    profilingManager       = profilingManager,
    libraryConfig          = libraryConfig,
    config                 = languageServerConfig
  )(system)
  log.trace(
    "Created JSON connection controller factory [{}]",
    jsonRpcControllerFactory
  )

  val secureConfig = SecureConnectionConfig
    .fromApplicationConfig(akkaHttpsConfig())
    .fold(
      v => v.flatMap(msg => { log.warn(s"invalid secure config: $msg"); None }),
      Some(_)
    )

  val materializer: Materializer = Materializer.createMaterializer(system)
  val jsonRpcServer =
    new JsonRpcServer(
      jsonRpcProtocolFactory,
      jsonRpcControllerFactory,
      JsonRpcServer
        .Config(
          outgoingBufferSize = 10000,
          lazyMessageTimeout = 10.seconds,
          secureConfig       = secureConfig
        ),
      List(healthCheckEndpoint, idlenessEndpoint),
      messagesCallback
    )(system, materializer)
  log.trace("Created JSON RPC Server [{}]", jsonRpcServer)

  val binaryServer =
    new BinaryWebSocketServer(
      InboundMessageDecoder,
      BinaryEncoder.empty,
      new BinaryConnectionControllerFactory(fileManager)(system),
      BinaryWebSocketServer.Config(
        outgoingBufferSize = 100,
        lazyMessageTimeout = 10.seconds,
        secureConfig       = secureConfig
      ),
      messagesCallback
    )(system, materializer)
  log.trace("Created Binary WebSocket Server [{}]", binaryServer)

  log.debug(
    "Main module of the Language Server initialized with config [{}]",
    languageServerConfig
  )

  /** Close the main module releasing all resources. */
  def close(): Unit = {
    suggestionsRepo.close()
    contextSupervisor.close()
    runtimeEventsMonitor.close()
    ydocSupervisor.close()
    log.info("Stopped Language Server")
  }

  private def akkaHttpsConfig(): com.typesafe.config.Config = {
    val empty = ConfigFactory.empty().atPath("akka.https")
    ConfigFactory
      .load()
      .withFallback(empty)
      .getConfig("akka")
      .getConfig("https")
  }
}
