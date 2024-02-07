package org.enso.languageserver.websocket.json

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import io.circe.literal._
import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import org.apache.commons.io.FileUtils
import org.enso.distribution.locking.ResourceManager
import org.enso.distribution.{DistributionManager, LanguageHome}
import org.enso.editions.updater.EditionManager
import org.enso.editions.{EditionResolver, Editions}
import org.enso.filewatcher.{NoopWatcherFactory, WatcherAdapterFactory}
import org.enso.jsonrpc.test.JsonRpcServerTestKit
import org.enso.jsonrpc.{ClientControllerFactory, ProtocolFactory}
import org.enso.languageserver.TestClock
import org.enso.languageserver.boot.{
  ProfilingConfig,
  StartupConfig,
  TimingsConfig
}
import org.enso.languageserver.boot.resource.{
  DirectoriesInitialization,
  InitializationComponent,
  RepoInitialization,
  SequentialResourcesInitialization,
  ZioRuntimeInitialization
}
import org.enso.languageserver.capability.CapabilityRouter
import org.enso.languageserver.data._
import org.enso.languageserver.effect.{ExecutionContextRuntime, ZioExec}
import org.enso.languageserver.event.InitializedEvent
import org.enso.languageserver.filemanager._
import org.enso.languageserver.io._
import org.enso.languageserver.libraries._
import org.enso.languageserver.monitoring.IdlenessMonitor
import org.enso.languageserver.profiling.{
  ProfilingManager,
  TestProfilingSnapshot
}
import org.enso.languageserver.protocol.json.{
  JsonConnectionControllerFactory,
  JsonRpcProtocolFactory
}
import org.enso.languageserver.runtime.{ContextRegistry, RuntimeFailureMapper}
import org.enso.languageserver.search.SuggestionsHandler
import org.enso.languageserver.search.SuggestionsHandler.ProjectNameUpdated
import org.enso.languageserver.session.SessionRouter
import org.enso.languageserver.text.BufferRegistry
import org.enso.languageserver.vcsmanager.{Git, VcsManager}
import org.enso.librarymanager.LibraryLocations
import org.enso.librarymanager.local.DefaultLocalLibraryProvider
import org.enso.librarymanager.published.PublishedLibraryCache
import org.enso.pkg.PackageManager
import org.enso.polyglot.data.TypeGraph
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.runtimeversionmanager.test.{
  FakeEnvironment,
  TestableThreadSafeFileLockManager
}
import org.enso.searcher.sql.{SqlDatabase, SqlSuggestionsRepo}
import org.enso.testkit.{EitherValue, WithTemporaryDirectory}
import org.enso.text.Sha3_224VersionCalculator
import org.scalactic.source
import org.scalatest.OptionValues
import org.slf4j.event.Level

import java.io.File
import java.net.URISyntaxException
import java.nio.file.{Files, Path}
import java.util.UUID
import java.util.concurrent.{Executors, ThreadFactory}
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

abstract class BaseServerTest
    extends JsonRpcServerTestKit
    with EitherValue
    with OptionValues
    with WithTemporaryDirectory
    with FakeEnvironment {

  val timeout: FiniteDuration = 10.seconds

  def isFileWatcherEnabled: Boolean = false

  val testContentRootId = UUID.randomUUID()
  val testContentRoot = ContentRootWithFile(
    ContentRoot.Project(testContentRootId),
    Files.createTempDirectory(null).toRealPath().toFile
  )
  val config                = mkConfig
  val runtimeConnectorProbe = TestProbe()
  val versionCalculator     = Sha3_224VersionCalculator
  val clock                 = TestClock()
  val profilingSnapshot     = new TestProfilingSnapshot

  val typeGraph: TypeGraph = {
    val graph = TypeGraph("Any")
    graph.insert("Number", "Any")
    graph.insert("Integer", "Number")
    graph
  }

  sys.addShutdownHook(FileUtils.deleteQuietly(testContentRoot.file))

  def mkConfig: Config =
    Config(
      testContentRoot,
      FileManagerConfig(timeout = 3.seconds),
      VcsManagerConfig(),
      PathWatcherConfig(),
      ExecutionContextConfig(requestTimeout = 3.seconds),
      ProjectDirectoriesConfig(testContentRoot.file),
      ProfilingConfig(),
      StartupConfig(),
      None
    )

  override def protocolFactory: ProtocolFactory =
    new JsonRpcProtocolFactory

  val stdOut    = new ObservableOutputStream
  val stdErr    = new ObservableOutputStream
  val stdInSink = new ObservableOutputStream
  val stdIn     = new ObservablePipedInputStream(stdInSink)

  val sessionRouter =
    system.actorOf(SessionRouter.props())

  val stdOutController =
    system.actorOf(
      OutputRedirectionController
        .props(stdOut, OutputKind.StandardOutput, sessionRouter)
    )

  val stdErrController =
    system.actorOf(
      OutputRedirectionController
        .props(stdErr, OutputKind.StandardError, sessionRouter)
    )

  val stdInController =
    system.actorOf(
      InputRedirectionController.props(stdIn, stdInSink, sessionRouter)
    )

  class TestThreadFactory(name: String) extends ThreadFactory {
    private val counter = new AtomicInteger(0)
    override def newThread(r: Runnable): Thread = {
      val t = new Thread();
      t.setName(name + "-" + counter.getAndIncrement())
      t
    }
  }

  val initThreadPool = Executors.newWorkStealingPool(4)
  val threadPool =
    Executors.newWorkStealingPool(10)
  val testExecutor = ExecutionContext.fromExecutor(threadPool)
  val zioRuntime   = new ExecutionContextRuntime(testExecutor)

  val zioExec         = ZioExec(zioRuntime)
  val sqlDatabase     = SqlDatabase(config.directories.suggestionsDatabaseFile)
  val suggestionsRepo = new SqlSuggestionsRepo(sqlDatabase)(system.dispatcher)

  private def initializationComponent =
    new SequentialResourcesInitialization(
      initThreadPool,
      new DirectoriesInitialization(initThreadPool, config.directories),
      new ZioRuntimeInitialization(
        initThreadPool,
        zioRuntime,
        system.eventStream
      ),
      new RepoInitialization(
        initThreadPool,
        config.directories,
        system.eventStream,
        sqlDatabase,
        suggestionsRepo
      )
    )

  val contentRootManagerActor =
    system.actorOf(ContentRootManagerActor.props(config))

  var cleanupCallbacks: List[() => Unit] = Nil

  var timingsConfig = TimingsConfig.default()

  override def afterEach(): Unit = {
    cleanupCallbacks.foreach(_())
    cleanupCallbacks = Nil
    timingsConfig    = TimingsConfig.default()
    super.afterEach()
  }

  override def afterAll(): Unit = {
    suggestionsRepo.close()
    threadPool.shutdown()
    initThreadPool.shutdown()
    super.afterAll()
  }

  /** Locates the root of the Enso repository. Heuristic: we just keep going up the directory tree
    * until we are in a directory containing ".git" subdirectory. Note that we cannot use the "enso"
    * name, as users are free to name their cloned directories however they like.
    */
  protected def locateRootDirectory(): File = {
    var rootDir: File = null
    try {
      rootDir = new File(
        classOf[
          BaseServerTest
        ].getProtectionDomain.getCodeSource.getLocation.toURI
      )
    } catch {
      case e: URISyntaxException =>
        fail("repository root directory not found: " + e.getMessage)
    }
    while (rootDir != null && !Files.exists(rootDir.toPath.resolve(".git"))) {
      rootDir = rootDir.getParentFile
    }
    if (rootDir == null) {
      fail("repository root directory not found")
    }
    rootDir
  }

  override def clientControllerFactory(): ClientControllerFactory = {
    val contentRootManagerWrapper: ContentRootManager =
      new ContentRootManagerWrapper(config, contentRootManagerActor)

    val fileManager = system.actorOf(
      FileManager.props(
        config.fileManager,
        contentRootManagerWrapper,
        new FileSystem,
        zioExec
      ),
      s"file-manager-${UUID.randomUUID()}"
    )
    val vcsManager = system.actorOf(
      VcsManager.props(
        config.vcsManager,
        Git.withEmptyUserConfig(
          Some(config.vcsManager.dataDirectory),
          config.vcsManager.asyncInit
        ),
        contentRootManagerWrapper,
        zioExec
      ),
      s"vcs-manager-${UUID.randomUUID()}"
    )
    val bufferRegistry =
      system.actorOf(
        BufferRegistry.props(
          fileManager,
          vcsManager,
          runtimeConnectorProbe.ref,
          contentRootManagerWrapper,
          timingsConfig
        )(
          Sha3_224VersionCalculator
        ),
        s"buffer-registry-${UUID.randomUUID()}"
      )
    val watcherFactory =
      if (isFileWatcherEnabled) new WatcherAdapterFactory
      else new NoopWatcherFactory
    val fileEventRegistry = system.actorOf(
      ReceivesTreeUpdatesHandler.props(
        config,
        contentRootManagerWrapper,
        watcherFactory,
        new FileSystem,
        zioExec
      ),
      s"fileevent-registry-${UUID.randomUUID()}"
    )

    val idlenessMonitor = system.actorOf(
      IdlenessMonitor.props(clock)
    )

    val contextRegistry =
      system.actorOf(
        ContextRegistry.props(
          config,
          RuntimeFailureMapper(contentRootManagerWrapper),
          runtimeConnectorProbe.ref,
          sessionRouter
        ),
        s"context-registry-${UUID.randomUUID()}"
      )

    val suggestionsHandler =
      system.actorOf(
        SuggestionsHandler.props(
          config,
          contentRootManagerWrapper,
          suggestionsRepo,
          sessionRouter,
          runtimeConnectorProbe.ref
        ),
        s"suggestions-handler-${UUID.randomUUID()}"
      )

    val capabilityRouter =
      system.actorOf(
        CapabilityRouter.props(
          bufferRegistry,
          fileEventRegistry,
          suggestionsHandler
        ),
        s"capability-router-${UUID.randomUUID()}"
      )

    // initialize
    suggestionsHandler ! InitializedEvent.TruffleContextInitialized
    runtimeConnectorProbe.receiveN(1)
    suggestionsHandler ! Api.Response(
      UUID.randomUUID(),
      Api.GetTypeGraphResponse(typeGraph)
    )
    initializationComponent.init().get(timeout.length, timeout.unit)
    suggestionsHandler ! ProjectNameUpdated("Test")

    val environment = fakeInstalledEnvironment()
    val languageHomePath =
      locateRootDirectory().toPath.resolve("distribution").resolve("component")
    val languageHome = LanguageHome(languageHomePath)
    languageHome.rootPath.toFile.exists() shouldBe true
    val distributionManager = new DistributionManager(environment)
    val lockManager: TestableThreadSafeFileLockManager =
      new TestableThreadSafeFileLockManager(distributionManager.paths.locks)

    // This is needed to be able to safely remove the temporary test directory on Windows.
    cleanupCallbacks ::= { () => lockManager.releaseAllLocks() }

    val resourceManager = new ResourceManager(lockManager)

    val editionProvider =
      EditionManager.makeEditionProvider(
        distributionManager,
        Some(languageHome)
      )
    val editionResolver = EditionResolver(editionProvider)
    val editionReferenceResolver = new EditionReferenceResolver(
      config.projectContentRoot.file,
      editionProvider,
      editionResolver
    )
    val editionManager = EditionManager(distributionManager, Some(languageHome))

    val projectSettingsManager = system.actorOf(
      ProjectSettingsManager.props(
        config.projectContentRoot.file,
        editionResolver
      )
    )

    val libraryLocations =
      LibraryLocations.resolve(
        distributionManager,
        Some(languageHome),
        Some(config.projectContentRoot.file.toPath)
      )

    val localLibraryManager = system.actorOf(
      LocalLibraryManager.props(
        config.projectContentRoot.file,
        libraryLocations
      )
    )

    val profilingManager = system.actorOf(
      ProfilingManager.props(
        runtimeConnectorProbe.ref,
        distributionManager,
        profilingSnapshot,
        clock
      )
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
        new CompilerBasedDependencyExtractor(logLevel = Level.WARN)
      )
    )

    new TestJsonConnectionControllerFactory(
      mainComponent          = initializationComponent,
      bufferRegistry         = bufferRegistry,
      capabilityRouter       = capabilityRouter,
      fileEventRegistry      = fileEventRegistry,
      fileManager            = fileManager,
      vcsManager             = vcsManager,
      contentRootManager     = contentRootManagerActor,
      contextRegistry        = contextRegistry,
      suggestionsHandler     = suggestionsHandler,
      stdOutController       = stdOutController,
      stdErrController       = stdErrController,
      stdInController        = stdInController,
      runtimeConnector       = runtimeConnectorProbe.ref,
      idlenessMonitor        = idlenessMonitor,
      projectSettingsManager = projectSettingsManager,
      profilingManager       = profilingManager,
      libraryConfig          = libraryConfig,
      config                 = config
    )
  }

  /** As we are testing the language server, we want to imitate the location
    * context of the runner.jar, while the default implementation of this method
    * was more suited towards testing the launcher.
    */
  override def fakeExecutablePath(portable: Boolean): Path =
    Path.of("distribution/component/runner/runner.jar")

  /** Specifies if the `package.yaml` at project root should be auto-created. */
  protected def initializeProjectPackage: Boolean = true

  /** Allows to customize the edition used by the project.
    *
    * Only applicable if [[initializeProjectPackage]] is [[true]].
    */
  protected def customEdition: Option[Editions.RawEdition] = None

  lazy val initPackage: Unit = {
    if (initializeProjectPackage) {
      PackageManager.Default.create(
        config.projectContentRoot.file,
        name    = "TestProject",
        edition = customEdition
      )
    }
  }

  def getInitialisedWsClient(debug: Boolean = false): WsTestClient = {
    val client = new WsTestClient(address, debugMessages = debug)
    initSession(client)
    client
  }

  def getInitialisedWsClientAndId(
    debug: Boolean = false
  ): (WsTestClient, ClientId) = {
    val client = new WsTestClient(address, debugMessages = debug)
    val uuid   = initSession(client)
    (client, uuid)
  }

  private def initSession(client: WsTestClient): UUID = {
    initPackage
    val clientId = UUID.randomUUID()
    client.send(json"""
          { "jsonrpc": "2.0",
            "method": "session/initProtocolConnection",
            "id": 1,
            "params": {
              "clientId": $clientId
            }
          }
          """)
    val response = parse(client.expectMessage()).rightValue.asObject.value
    response("jsonrpc") shouldEqual Some("2.0".asJson)
    response("id") shouldEqual Some(1.asJson)
    val result = response("result").value.asObject.value
    result("contentRoots").value.asArray.value should contain(
      json"""
          {
            "id" : $testContentRootId,
            "type" : "Project"
          }
          """
    )
    clientId
  }

  def receiveAndReplyToOpenFile()(implicit pos: source.Position): Unit = {
    receiveAndReplyToOpenFile(None)
  }
  def receiveAndReplyToOpenFile(
    fileName: String
  )(implicit pos: source.Position): Unit = {
    receiveAndReplyToOpenFile(Some(fileName))
  }
  private def receiveAndReplyToOpenFile(
    fileName: Option[String]
  )(implicit pos: source.Position): Unit = {
    runtimeConnectorProbe.receiveN(1).head match {
      case Api.Request(requestId, Api.OpenFileRequest(file, _)) =>
        fileName match {
          case Some(f) if f != file.getName =>
            fail(
              "expected OpenFile notification for `" + f + "`, got it for `" + file.getName + "`"
            )
          case _ =>
        }
        runtimeConnectorProbe.lastSender ! Api.Response(
          requestId,
          Api.OpenFileResponse
        )
      case Api.Request(
            _,
            _: Api.GetTypeGraphRequest | _: Api.EditFileNotification |
            _: Api.CloseFileNotification
          ) =>
        // ignore
        receiveAndReplyToOpenFile(fileName)
      case msg =>
        fail("expected OpenFile notification got " + msg)
    }
  }

  class TestJsonConnectionControllerFactory(
    mainComponent: InitializationComponent,
    bufferRegistry: ActorRef,
    capabilityRouter: ActorRef,
    fileEventRegistry: ActorRef,
    fileManager: ActorRef,
    vcsManager: ActorRef,
    contentRootManager: ActorRef,
    contextRegistry: ActorRef,
    suggestionsHandler: ActorRef,
    stdOutController: ActorRef,
    stdErrController: ActorRef,
    stdInController: ActorRef,
    runtimeConnector: ActorRef,
    idlenessMonitor: ActorRef,
    projectSettingsManager: ActorRef,
    profilingManager: ActorRef,
    libraryConfig: LibraryConfig,
    config: Config
  )(implicit system: ActorSystem)
      extends JsonConnectionControllerFactory(
        mainComponent,
        bufferRegistry,
        capabilityRouter,
        fileManager,
        vcsManager,
        contentRootManager,
        contextRegistry,
        suggestionsHandler,
        stdOutController,
        stdErrController,
        stdInController,
        runtimeConnector,
        idlenessMonitor,
        projectSettingsManager,
        profilingManager,
        libraryConfig,
        config
      )(system) {
    override def shutdown(): Unit = {
      fileEventRegistry ! ReceivesTreeUpdatesHandler.Stop
      super.shutdown()
    }
  }
}
