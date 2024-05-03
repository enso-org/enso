package org.enso.languageserver.boot

import akka.actor.ActorSystem
import buildinfo.Info
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
import org.enso.languageserver.profiling.ProfilingManager
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
import org.enso.logger.Converter
import org.enso.logger.masking.Masking
import org.enso.logger.JulHandler
import org.enso.logger.akka.AkkaConverter
import org.enso.common.HostAccessFactory
import org.enso.polyglot.{RuntimeOptions, RuntimeServerInfo}
import org.enso.profiling.events.NoopEventsMonitor
import org.enso.searcher.memory.InMemorySuggestionsRepo
import org.enso.text.{ContentBasedVersioning, Sha3_224VersionCalculator}
import org.enso.ydoc.Ydoc
import org.graalvm.polyglot.Engine
import org.graalvm.polyglot.Context
import org.graalvm.polyglot.io.MessageEndpoint
import org.slf4j.event.Level
import org.slf4j.LoggerFactory

import java.io.{File, PrintStream}
import java.net.URI
import java.nio.charset.StandardCharsets
import java.time.Clock

import scala.concurrent.duration._

/** A main module containing all components of the server.
  *
  * @param serverConfig configuration for the language server
  * @param logLevel log level for the Language Server
  */
class MainModule(serverConfig: LanguageServerConfig, logLevel: Level) {

  private val log = LoggerFactory.getLogger(this.getClass)
  log.info(
    "Initializing main module of the Language Server from [{}, {}, {}]",
    Info.currentEdition,
    serverConfig,
    logLevel
  )

  private val utcClock = Clock.systemUTC()

  val directoriesConfig = ProjectDirectoriesConfig(serverConfig.contentRootPath)
  private val contentRoot = ContentRootWithFile(
    ContentRoot.Project(serverConfig.contentRootUuid),
    new File(serverConfig.contentRootPath)
  )

  private val openAiKey = sys.env.get("OPENAI_API_KEY")
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
  log.trace("Created Language Server config [{}].", languageServerConfig)

  implicit val system: ActorSystem =
    ActorSystem(
      serverConfig.name,
      None,
      Some(getClass.getClassLoader),
      Some(serverConfig.computeExecutionContext)
    )
  log.trace("Created ActorSystem [{}].", system)

  private val zioRuntime =
    new effect.ExecutionContextRuntime(system.dispatcher)
  private val zioExec = effect.ZioExec(zioRuntime)
  log.trace("Created ZIO executor [{}].", zioExec)

  private val fileSystem: FileSystem = new FileSystem
  log.trace("Created file system [{}].", fileSystem)

  val git = Git.withEmptyUserConfig(
    Some(languageServerConfig.vcsManager.dataDirectory),
    languageServerConfig.vcsManager.asyncInit
  )
  log.trace("Created git [{}].", git)

  implicit val versionCalculator: ContentBasedVersioning =
    Sha3_224VersionCalculator
  log.trace("Created Version Calculator [{}].", versionCalculator)

  val suggestionsRepo =
    new InMemorySuggestionsRepo()(
      system.dispatcher
    );
  log.trace("Created SQL suggestions repo: [{}].", suggestionsRepo)

  val idlenessMonitor =
    system.actorOf(IdlenessMonitor.props(utcClock))

  lazy val sessionRouter =
    system.actorOf(SessionRouter.props(), "session-router")

  val environment         = new Environment {}
  val languageHome        = LanguageHome.detectFromExecutableLocation(environment)
  val distributionManager = new DistributionManager(environment)

  val editionProvider =
    EditionManager.makeEditionProvider(distributionManager, Some(languageHome))
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

  val runtimeEventsMonitor =
    languageServerConfig.profiling.profilingEventsLogPath match {
      case Some(path) =>
        val out = new PrintStream(path.toFile, StandardCharsets.UTF_8)
        new RuntimeEventsMonitor(out)
      case None =>
        new NoopEventsMonitor()
    }
  log.trace(
    "Started runtime events monitor [{}].",
    runtimeEventsMonitor.getClass.getName
  )

  lazy val runtimeConnector =
    system.actorOf(
      RuntimeConnector.props(lockManagerService, runtimeEventsMonitor),
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
    FileManager.pool(
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

  val builder = Context
    .newBuilder()
    .allowAllAccess(true)
    .allowHostAccess(new HostAccessFactory().allWithTypeMapping())
    .allowExperimentalOptions(true)
    .option(RuntimeServerInfo.ENABLE_OPTION, "true")
    .option(RuntimeOptions.INTERACTIVE_MODE, "true")
    .option(RuntimeOptions.PROJECT_ROOT, serverConfig.contentRootPath)
    .option(
      RuntimeOptions.LOG_LEVEL,
      Converter.toJavaLevel(logLevel).getName
    )
    .option(RuntimeOptions.LOG_MASKING, Masking.isMaskingEnabled.toString)
    .option(RuntimeOptions.EDITION_OVERRIDE, Info.currentEdition)
    .option(
      RuntimeOptions.JOB_PARALLELISM,
      Runtime.getRuntime.availableProcessors().toString
    )
    .option(RuntimeOptions.PREINITIALIZE, "js")
    .out(stdOut)
    .err(stdErr)
    .in(stdIn)
    .logHandler(JulHandler.get())
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
  if (
    Engine
      .newBuilder()
      .allowExperimentalOptions(true)
      .build
      .getLanguages()
      .containsKey("java")
  ) {
    builder
      .option("java.ExposeNativeJavaVM", "true")
      .option("java.Polyglot", "true")
      .option("java.UseBindingsLoader", "true")
      .allowCreateThread(true)
  }

  val context = builder.build()
  log.trace("Created Runtime context [{}].", context)

  system.eventStream.setLogLevel(AkkaConverter.toAkka(logLevel))
  log.trace("Set akka log level to [{}].", logLevel)

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

  val projectSettingsManager = system.actorOf(
    ProjectSettingsManager.props(contentRoot.file, editionResolver),
    "project-settings-manager"
  )

  val profilingManager =
    system.actorOf(
      ProfilingManager.props(runtimeConnector, distributionManager),
      "profiling-manager"
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
      context,
      zioRuntime
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
  )
  log.trace(
    "Created JSON connection controller factory [{}].",
    jsonRpcControllerFactory
  )

  val secureConfig = SecureConnectionConfig
    .fromApplicationConfig(applicationConfig())
    .fold(
      v => v.flatMap(msg => { log.warn(s"invalid secure config: $msg"); None }),
      Some(_)
    )

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
      List(healthCheckEndpoint, idlenessEndpoint)
    )
  log.trace("Created JSON RPC Server [{}].", jsonRpcServer)

  val binaryServer =
    new BinaryWebSocketServer(
      InboundMessageDecoder,
      BinaryEncoder.empty,
      new BinaryConnectionControllerFactory(fileManager),
      BinaryWebSocketServer.Config(
        outgoingBufferSize = 100,
        lazyMessageTimeout = 10.seconds,
        secureConfig       = secureConfig
      )
    )
  log.trace("Created Binary WebSocket Server [{}].", binaryServer)

  private val ydoc = new Ydoc()
  ydoc.getContextBuilder.logHandler(JulHandler.get())
  ydoc.start()
  log.trace("Started Ydoc server.")

  log.info(
    "Main module of the Language Server initialized with config [{}].",
    languageServerConfig
  )

  /** Close the main module releasing all resources. */
  def close(): Unit = {
    suggestionsRepo.close()
    context.close()
    ydoc.close()
    log.info("Closed Language Server main module.")
  }

  private def applicationConfig(): com.typesafe.config.Config = {
    val empty = ConfigFactory.empty().atPath("akka.https")
    ConfigFactory
      .load()
      .withFallback(empty)
      .getConfig("akka")
      .getConfig("https")
  }
}
