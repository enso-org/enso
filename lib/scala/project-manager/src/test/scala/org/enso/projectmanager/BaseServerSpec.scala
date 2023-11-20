package org.enso.projectmanager

import java.io.File
import java.nio.file.{Files, Path}
import java.time.{OffsetDateTime, ZoneOffset}
import java.util.UUID
import akka.testkit.TestActors.blackholeProps
import akka.testkit._
import io.circe.Json
import io.circe.parser.parse
import nl.gn0s1s.bump.SemVer
import org.apache.commons.io.FileUtils
import org.enso.distribution.FileSystem.PathSyntax
import org.enso.distribution.FileSystem
import org.enso.editions.Editions
import org.enso.cli.OS
import org.enso.jsonrpc.test.JsonRpcServerTestKit
import org.enso.jsonrpc.{ClientControllerFactory, ProtocolFactory}
import org.enso.logger.LoggerSetup
import org.enso.pkg.{Config, PackageManager}
import org.enso.projectmanager.boot.Globals.{ConfigFilename, ConfigNamespace}
import org.enso.projectmanager.boot.configuration._
import org.enso.projectmanager.control.effect.ZioEnvExec
import org.enso.projectmanager.data.MissingComponentAction
import org.enso.projectmanager.infrastructure.file.BlockingFileSystem
import org.enso.projectmanager.infrastructure.languageserver.{
  ExecutorWithUnlimitedPool,
  LanguageServerGatewayImpl,
  LanguageServerRegistry,
  ShutdownHookActivator
}
import org.enso.projectmanager.infrastructure.log.Slf4jLogging
import org.enso.projectmanager.infrastructure.repository.ProjectFileRepository
import org.enso.projectmanager.protocol.{
  JsonRpcProtocolFactory,
  ManagerClientControllerFactory
}
import org.enso.projectmanager.service.config.GlobalConfigService
import org.enso.projectmanager.service.validation.ProjectNameValidator
import org.enso.projectmanager.service.versionmanagement.{
  RuntimeVersionManagementService,
  RuntimeVersionManagerFactory
}
import org.enso.projectmanager.service.{ProjectCreationService, ProjectService}
import org.enso.projectmanager.test.{ObservableGenerator, ProgrammableClock}
import org.enso.runtimeversionmanager.CurrentVersion
import org.enso.runtimeversionmanager.components.GraalVMVersion
import org.enso.runtimeversionmanager.test.FakeReleases
import org.scalatest.BeforeAndAfterAll
import org.slf4j.event.Level
import pureconfig.ConfigSource
import pureconfig.generic.auto._
import zio.interop.catz.core._
import zio.{Runtime, Semaphore, ZAny, ZIO}

import java.net.URISyntaxException
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class BaseServerSpec extends JsonRpcServerTestKit with BeforeAndAfterAll {

  /** Tests can override this value to request a specific engine version to be
    * preinstalled when running the suite.
    */
  val engineToInstall: Option[SemVer] = None

  /** Tests can override this to set up a logging service that will print debug
    * logs.
    */
  val debugLogs: Boolean = false

  /** Enables the application profiling. */
  val profilingPath: Option[Path] = None

  /** Tests can override this to allow child process output to be displayed. */
  val debugChildLogs: Boolean = false

  override def protocolFactory: ProtocolFactory =
    new JsonRpcProtocolFactory

  val config: ProjectManagerConfig =
    ConfigSource
      .resources(ConfigFilename)
      .withFallback(ConfigSource.systemProperties)
      .at(ConfigNamespace)
      .loadOrThrow[ProjectManagerConfig]

  val processConfig: MainProcessConfig =
    MainProcessConfig(
      logLevel      = if (debugLogs) Level.TRACE else Level.ERROR,
      profilingPath = profilingPath,
      profilingTime = None
    )

  val testClock =
    new ProgrammableClock[ZAny](OffsetDateTime.now(ZoneOffset.UTC))

  def getGeneratedUUID: UUID = {
    Await.result(Future(gen.takeFirst())(system.dispatcher), 3.seconds.dilated)
  }

  lazy val gen = new ObservableGenerator[ZAny]()

  val testProjectsRoot = Files.createTempDirectory(null).toFile
  sys.addShutdownHook(FileUtils.deleteQuietly(testProjectsRoot))

  val testDistributionRoot = Files.createTempDirectory(null).toFile
  sys.addShutdownHook(FileUtils.deleteQuietly(testDistributionRoot))

  val userProjectDir = new File(testProjectsRoot, "projects")

  lazy val testStorageConfig = StorageConfig(
    projectsRoot             = testProjectsRoot,
    userProjectsPath         = userProjectDir,
    projectMetadataDirectory = ".enso",
    projectMetadataFileName  = "project.json"
  )

  lazy val bootloaderConfig = config.bootloader

  lazy val timeoutConfig = config.timeout

  lazy val netConfig = config.network

  lazy val supervisionConfig = config.supervision

  implicit val exec: ZioEnvExec = new ZioEnvExec(Runtime.default)

  lazy val fileSystem = new BlockingFileSystem(5.seconds)

  lazy val storageSemaphore =
    zio.Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(Semaphore.make(1)).getOrThrow()
    }

  lazy val projectRepository =
    new ProjectFileRepository(
      testStorageConfig,
      testClock,
      fileSystem,
      gen
    )

  lazy val projectNameValidator = new ProjectNameValidator[ZIO[ZAny, *, *]]()

  val distributionConfiguration =
    TestDistributionConfiguration(
      distributionRoot       = testDistributionRoot.toPath,
      engineReleaseProvider  = FakeReleases.engineReleaseProvider,
      runtimeReleaseProvider = FakeReleases.runtimeReleaseProvider,
      discardChildOutput     = !debugChildLogs
    )

  val loggingService = new TestLoggingService

  lazy val languageServerRegistry =
    system.actorOf(
      LanguageServerRegistry
        .props(
          netConfig,
          bootloaderConfig,
          supervisionConfig,
          timeoutConfig,
          distributionConfiguration,
          processConfig,
          loggingService,
          ExecutorWithUnlimitedPool
        )
    )

  lazy val shutdownHookActivator =
    system.actorOf(ShutdownHookActivator.props[ZIO[ZAny, +*, +*]]())

  lazy val languageServerGateway =
    new LanguageServerGatewayImpl[ZIO[ZAny, +*, +*]](
      languageServerRegistry,
      shutdownHookActivator,
      system,
      timeoutConfig
    )

  lazy val projectCreationService =
    new ProjectCreationService[ZIO[ZAny, +*, +*]](
      distributionConfiguration,
      loggingService
    )

  lazy val globalConfigService = new GlobalConfigService[ZIO[ZAny, +*, +*]](
    distributionConfiguration
  )

  lazy val projectService =
    new ProjectService[ZIO[ZAny, +*, +*]](
      projectNameValidator,
      projectRepository,
      projectCreationService,
      globalConfigService,
      new Slf4jLogging[ZIO[ZAny, +*, +*]],
      testClock,
      gen,
      languageServerGateway,
      distributionConfiguration
    )

  lazy val runtimeVersionManagementService =
    new RuntimeVersionManagementService[ZIO[ZAny, +*, +*]](
      distributionConfiguration
    )

  override def clientControllerFactory: ClientControllerFactory = {
    new ManagerClientControllerFactory[ZIO[ZAny, +*, +*]](
      system                          = system,
      projectService                  = projectService,
      globalConfigService             = globalConfigService,
      runtimeVersionManagementService = runtimeVersionManagementService,
      loggingServiceDescriptor        = loggingService,
      timeoutConfig                   = timeoutConfig
    )
  }

  /** Can be used to avoid deleting the project's root. */
  val deleteProjectsRootAfterEachTest = true

  override def afterEach(): Unit = {
    super.afterEach()

    if (deleteProjectsRootAfterEachTest)
      FileUtils.deleteQuietly(testProjectsRoot)
  }

  override def afterAll(): Unit = {
    super.afterAll()

    FileUtils.deleteQuietly(testProjectsRoot)
  }

  override def beforeAll(): Unit = {
    super.beforeAll()

    if (debugLogs) {
      LoggerSetup.get().setup(Level.TRACE)
    } else {
      LoggerSetup.get().setup()
    }

    setupEditions()

    engineToInstall.foreach(preInstallEngine)
  }

  private def setupEditions(): Unit = {
    val engineVersion = engineToInstall.getOrElse(CurrentVersion.version)
    val editionsDir   = testDistributionRoot.toPath / "test_data" / "editions"
    Files.createDirectories(editionsDir)
    val editionName = buildinfo.Info.currentEdition + ".yaml"
    val editionConfig =
      s"""engine-version: $engineVersion
         |""".stripMargin
    FileSystem.writeTextFile(editionsDir / editionName, editionConfig)
  }

  /** Locates the root of the Enso repository. Heuristic: we just keep going up the directory tree
    * until we are in a directory containing ".git" subdirectory. Note that we cannot use the "enso"
    * name, as users are free to name their cloned directories however they like.
    */
  private def locateRootDirectory(): File = {
    var rootDir: File = null
    try {
      rootDir = new File(
        classOf[
          BaseServerSpec
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

  /** This is a temporary solution to ensure that a valid engine distribution is
    * preinstalled.
    *
    * In the future the fake release mechanism can be properly updated to allow
    * for this kind of configuration without special logic.
    */
  def preInstallEngine(version: SemVer): Unit = {
    val os   = OS.operatingSystem.configName
    val ext  = if (OS.isWindows) "zip" else "tar.gz"
    val arch = OS.architecture
    val componentDestDir = FakeReleases.releaseRoot
      .resolve("enso")
      .resolve(s"enso-$version")
      .resolve(s"enso-engine-$version-$os-$arch.$ext")
      .resolve(s"enso-$version")
      .resolve("component")
    val root = locateRootDirectory().toPath.normalize()
    // Copy all the components from build engine distribution.
    val envMap     = System.getenv()
    val versionEnv = envMap.getOrDefault("ENSO_VERSION", "0.0.0-dev")
    val builtDistributionDir = root.resolve(
      s"built-distribution/enso-engine-$versionEnv-$os-$arch/enso-$versionEnv"
    )
    if (!builtDistributionDir.toFile.exists()) {
      throw new AssertionError(
        s"Expecting that engine distribution has already been built " +
        s"for project-manager tests: There is no directory $builtDistributionDir. We need to copy all the components from there."
      )
    }
    val componentsSourceDir = builtDistributionDir.resolve("component")
    FileUtils.copyDirectory(componentsSourceDir.toFile, componentDestDir.toFile)

    val blackhole = system.actorOf(blackholeProps)
    val installAction = runtimeVersionManagementService.installEngine(
      blackhole,
      version,
      forceInstallBroken = false
    )
    zio.Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(installAction)
    }
  }

  def uninstallEngine(version: SemVer): Unit = {
    val blackhole = system.actorOf(blackholeProps)
    val action = runtimeVersionManagementService.uninstallEngine(
      blackhole,
      version
    )
    zio.Unsafe.unsafe { implicit unsafe =>
      Runtime.default.unsafe.run(action)
    }
  }

  def uninstallRuntime(graalVMVersion: GraalVMVersion): Unit = {
    val blackhole = system.actorOf(blackholeProps)
    val runtimeVersionManager = RuntimeVersionManagerFactory(
      distributionConfiguration
    ).makeRuntimeVersionManager(blackhole, MissingComponentAction.Fail)
    val runtime = runtimeVersionManager.findGraalRuntime(graalVMVersion).get
    FileUtils.deleteDirectory(runtime.path.toFile)
  }

  implicit class ClientSyntax(client: WsTestClient) {
    def expectTaskStarted(
      timeout: FiniteDuration = 20.seconds.dilated
    ): Unit = {
      inside(parse(client.expectMessage(timeout))) { case Right(json) =>
        getMethod(json) shouldEqual Some("task/started")
      }
    }

    private def getMethod(json: Json): Option[String] = for {
      obj    <- json.asObject
      method <- obj("method").flatMap(_.asString)
    } yield method

    def expectJsonIgnoring(
      shouldIgnore: Json => Boolean,
      timeout: FiniteDuration = 20.seconds.dilated
    ): Json = {
      inside(parse(client.expectMessage(timeout))) { case Right(json) =>
        if (shouldIgnore(json)) expectJsonIgnoring(shouldIgnore, timeout)
        else json
      }
    }

    def expectError(
      expectedCode: Int,
      timeout: FiniteDuration = 10.seconds.dilated
    ): Unit = {
      withClue("Response should be an error: ") {
        inside(parse(client.expectMessage(timeout))) { case Right(json) =>
          val code = for {
            obj   <- json.asObject
            error <- obj("error").flatMap(_.asObject)
            code  <- error("code").flatMap(_.asNumber).flatMap(_.toInt)
          } yield code
          code shouldEqual Some(expectedCode)
        }
      }
    }

    def expectJsonAfterSomeProgress(
      json: Json,
      timeout: FiniteDuration = 10.seconds.dilated
    ): Unit =
      expectJsonIgnoring(
        json => getMethod(json).exists(_.startsWith("task/")),
        timeout
      ) shouldEqual json
  }

  /** Modifies the project's package.yaml. */
  def updateProjectConfig(
    projectName: String
  )(update: Config => Config): Unit = {
    val pkgFile = new File(userProjectDir, projectName)
    val pkg     = PackageManager.Default.loadPackage(pkgFile).get
    pkg.updateConfig(update)
  }

  /** Sets project's parent edition. */
  def setProjectParentEdition(
    projectName: String,
    newParentEditionName: String
  ): Unit = updateProjectConfig(projectName) { config =>
    config.copy(edition =
      Some(Editions.Raw.Edition(parent = Some(newParentEditionName)))
    )
  }
}
