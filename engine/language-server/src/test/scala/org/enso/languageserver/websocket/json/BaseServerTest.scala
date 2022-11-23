package org.enso.languageserver.websocket.json

import akka.testkit.TestProbe
import io.circe.literal._
import io.circe.parser.parse
import io.circe.syntax.EncoderOps
import org.apache.commons.io.FileUtils
import org.enso.distribution.locking.ResourceManager
import org.enso.distribution.{DistributionManager, LanguageHome}
import org.enso.editions.updater.EditionManager
import org.enso.editions.{EditionResolver, Editions}
import org.enso.jsonrpc.test.JsonRpcServerTestKit
import org.enso.jsonrpc.{ClientControllerFactory, Protocol}
import org.enso.languageserver.TestClock
import org.enso.languageserver.boot.{ProfilingConfig, TimingsConfig}
import org.enso.languageserver.boot.resource.{
  DirectoriesInitialization,
  RepoInitialization,
  SequentialResourcesInitialization
}
import org.enso.languageserver.capability.CapabilityRouter
import org.enso.languageserver.data._
import org.enso.languageserver.effect.ZioExec
import org.enso.languageserver.event.InitializedEvent
import org.enso.languageserver.filemanager._
import org.enso.languageserver.io._
import org.enso.languageserver.libraries._
import org.enso.languageserver.monitoring.IdlenessMonitor
import org.enso.languageserver.protocol.json.{
  JsonConnectionControllerFactory,
  JsonRpc
}
import org.enso.languageserver.refactoring.ProjectNameChangedEvent
import org.enso.languageserver.runtime.{ContextRegistry, RuntimeFailureMapper}
import org.enso.languageserver.search.SuggestionsHandler
import org.enso.languageserver.session.SessionRouter
import org.enso.languageserver.text.BufferRegistry
import org.enso.languageserver.vcsmanager.{Git, VcsManager}
import org.enso.librarymanager.LibraryLocations
import org.enso.librarymanager.local.DefaultLocalLibraryProvider
import org.enso.librarymanager.published.PublishedLibraryCache
import org.enso.loggingservice.LogLevel
import org.enso.pkg.PackageManager
import org.enso.polyglot.data.TypeGraph
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.runtimeversionmanager.test.{
  FakeEnvironment,
  TestableThreadSafeFileLockManager
}
import org.enso.searcher.sql.{SqlDatabase, SqlSuggestionsRepo, SqlVersionsRepo}
import org.enso.testkit.{EitherValue, WithTemporaryDirectory}
import org.enso.text.Sha3_224VersionCalculator
import org.scalatest.OptionValues

import java.nio.file.{Files, Path}
import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration._

class BaseServerTest
    extends JsonRpcServerTestKit
    with EitherValue
    with OptionValues
    with WithTemporaryDirectory
    with FakeEnvironment {

  import system.dispatcher

  val timeout: FiniteDuration = 10.seconds

  val testContentRootId = UUID.randomUUID()
  val testContentRoot = ContentRootWithFile(
    ContentRoot.Project(testContentRootId),
    Files.createTempDirectory(null).toRealPath().toFile
  )
  val config                = mkConfig
  val runtimeConnectorProbe = TestProbe()
  val versionCalculator     = Sha3_224VersionCalculator
  val clock                 = TestClock()

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
      VcsManagerConfig(timeout  = 5.seconds),
      PathWatcherConfig(),
      ExecutionContextConfig(requestTimeout = 3.seconds),
      ProjectDirectoriesConfig(testContentRoot.file),
      ProfilingConfig()
    )

  override def protocol: Protocol = JsonRpc.protocol

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

  val zioExec         = ZioExec(zio.Runtime.default)
  val sqlDatabase     = SqlDatabase(config.directories.suggestionsDatabaseFile)
  val suggestionsRepo = new SqlSuggestionsRepo(sqlDatabase)(system.dispatcher)
  val versionsRepo    = new SqlVersionsRepo(sqlDatabase)(system.dispatcher)

  val initializationComponent = SequentialResourcesInitialization(
    new DirectoriesInitialization(config.directories),
    new RepoInitialization(
      config.directories,
      system.eventStream,
      suggestionsRepo,
      versionsRepo
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

  override def clientControllerFactory: ClientControllerFactory = {
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
        Git.withEmptyUserConfig(),
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
          timingsConfig
        )(
          Sha3_224VersionCalculator
        ),
        s"buffer-registry-${UUID.randomUUID()}"
      )
    val fileEventRegistry = system.actorOf(
      ReceivesTreeUpdatesHandler.props(
        config,
        contentRootManagerWrapper,
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
          suggestionsRepo,
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
          versionsRepo,
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
    Await.ready(initializationComponent.init(), timeout)
    system.eventStream.publish(ProjectNameChangedEvent("Test", "Test"))
    runtimeConnectorProbe.receiveN(1)
    suggestionsHandler ! Api.Response(
      UUID.randomUUID(),
      Api.VerifyModulesIndexResponse(Seq())
    )

    val environment         = fakeInstalledEnvironment()
    val languageHome        = LanguageHome.detectFromExecutableLocation(environment)
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

    val localLibraryManager = system.actorOf(
      LocalLibraryManager.props(
        config.projectContentRoot.file,
        distributionManager
      )
    )

    val libraryLocations =
      LibraryLocations.resolve(distributionManager, Some(languageHome))

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
        new CompilerBasedDependencyExtractor(logLevel = LogLevel.Warning)
      )
    )

    new JsonConnectionControllerFactory(
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
      runtimeConnector       = runtimeConnectorProbe.ref,
      idlenessMonitor        = idlenessMonitor,
      projectSettingsManager = projectSettingsManager,
      libraryConfig          = libraryConfig,
      config                 = config
    )
  }

  /** As we are testing the language server, we want to imitate the location
    * context of the runner.jar, while the default implementation of this method
    * was more suited towards testing the launcher.
    */
  override def fakeExecutablePath(portable: Boolean): Path =
    Path.of("distribution/component/runner.jar")

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
}
