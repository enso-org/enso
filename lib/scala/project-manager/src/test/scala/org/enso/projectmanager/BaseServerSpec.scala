package org.enso.projectmanager

import java.io.File
import java.nio.file.{Files, Path}
import java.time.{OffsetDateTime, ZoneOffset}
import java.util.UUID

import akka.testkit.TestActors.blackholeProps
import akka.testkit._
import nl.gn0s1s.bump.SemVer
import org.apache.commons.io.FileUtils
import org.enso.jsonrpc.test.JsonRpcServerTestKit
import org.enso.jsonrpc.{ClientControllerFactory, Protocol}
import org.enso.projectmanager.boot.Globals.{ConfigFilename, ConfigNamespace}
import org.enso.projectmanager.boot.configuration._
import org.enso.projectmanager.control.effect.ZioEnvExec
import org.enso.projectmanager.infrastructure.file.BlockingFileSystem
import org.enso.projectmanager.infrastructure.languageserver.{
  LanguageServerGatewayImpl,
  LanguageServerRegistry,
  ShutdownHookActivator
}
import org.enso.projectmanager.infrastructure.log.Slf4jLogging
import org.enso.projectmanager.infrastructure.repository.ProjectFileRepository
import org.enso.projectmanager.protocol.{
  JsonRpc,
  ManagerClientControllerFactory
}
import org.enso.projectmanager.service.config.GlobalConfigService
import org.enso.projectmanager.service.versionmanagement.RuntimeVersionManagementService
import org.enso.projectmanager.service.{
  MonadicProjectValidator,
  ProjectCreationService,
  ProjectService
}
import org.enso.projectmanager.test.{ObservableGenerator, ProgrammableClock}
import org.enso.runtimeversionmanager.OS
import org.enso.runtimeversionmanager.runner.{JVMSettings, JavaCommand}
import org.enso.runtimeversionmanager.test.{DropLogs, FakeReleases}
import org.scalatest.BeforeAndAfterAll
import pureconfig.ConfigSource
import pureconfig.generic.auto._
import zio.interop.catz.core._
import zio.{Runtime, Semaphore, ZEnv, ZIO}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.jdk.OptionConverters.RichOptional

class BaseServerSpec
    extends JsonRpcServerTestKit
    with DropLogs
    with BeforeAndAfterAll {

  override def protocol: Protocol = JsonRpc.protocol

  val config: ProjectManagerConfig =
    ConfigSource
      .resources(ConfigFilename)
      .withFallback(ConfigSource.systemProperties)
      .at(ConfigNamespace)
      .loadOrThrow[ProjectManagerConfig]

  val testClock =
    new ProgrammableClock[ZEnv](OffsetDateTime.now(ZoneOffset.UTC))

  def getGeneratedUUID: UUID = {
    Await.result(Future(gen.takeFirst())(system.dispatcher), 3.seconds.dilated)
  }

  lazy val gen = new ObservableGenerator[ZEnv]()

  val testProjectsRoot = Files.createTempDirectory(null).toFile
  sys.addShutdownHook(FileUtils.deleteQuietly(testProjectsRoot))

  val testDistributionRoot = Files.createTempDirectory(null).toFile
  println(s"Rooot: ${testDistributionRoot.toPath.toAbsolutePath.normalize()}")
//  sys.addShutdownHook(FileUtils.deleteQuietly(testDistributionRoot))

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

  implicit val exec = new ZioEnvExec(Runtime.default)

  lazy val fileSystem = new BlockingFileSystem(5.seconds)

  lazy val storageSemaphore =
    Runtime.default.unsafeRun(Semaphore.make(1))

  lazy val projectRepository =
    new ProjectFileRepository(
      testStorageConfig,
      testClock,
      fileSystem,
      gen
    )

  lazy val projectValidator = new MonadicProjectValidator[ZIO[ZEnv, *, *]]()

  lazy val languageServerRegistry =
    system.actorOf(
      LanguageServerRegistry
        .props(netConfig, bootloaderConfig, supervisionConfig, timeoutConfig)
    )

  lazy val shutdownHookActivator =
    system.actorOf(ShutdownHookActivator.props[ZIO[ZEnv, +*, +*]]())

  lazy val languageServerGateway =
    new LanguageServerGatewayImpl[ZIO[ZEnv, +*, +*]](
      languageServerRegistry,
      shutdownHookActivator,
      system,
      timeoutConfig
    )

  lazy val distributionConfiguration =
    TestDistributionConfiguration(
      distributionRoot       = testDistributionRoot.toPath,
      engineReleaseProvider  = FakeReleases.engineReleaseProvider,
      runtimeReleaseProvider = FakeReleases.runtimeReleaseProvider
    )

  lazy val projectCreationService =
    new ProjectCreationService[ZIO[ZEnv, +*, +*]](distributionConfiguration) {

      /** Tests runner must use the system JVM to avoid installing GraalVM
        * inside of tests which would take far too long.
        */
      override val jvmSettings: JVMSettings = {
        val currentProcess =
          ProcessHandle.current().info().command().toScala.getOrElse("java")
        val javaCommand = JavaCommand(currentProcess, None)
        new JVMSettings(
          javaCommandOverride = Some(javaCommand),
          jvmOptions          = Seq()
        )
      }
    }

  lazy val projectService =
    new ProjectService[ZIO[ZEnv, +*, +*]](
      projectValidator,
      projectRepository,
      projectCreationService,
      new Slf4jLogging[ZIO[ZEnv, +*, +*]],
      testClock,
      gen,
      languageServerGateway
    )

  lazy val globalConfigService = new GlobalConfigService[ZIO[ZEnv, +*, +*]](
    distributionConfiguration
  )

  lazy val runtimeVersionManagementService =
    new RuntimeVersionManagementService[ZIO[ZEnv, +*, +*]](
      distributionConfiguration
    )

  override def clientControllerFactory: ClientControllerFactory = {
    new ManagerClientControllerFactory[ZIO[ZEnv, +*, +*]](
      system                          = system,
      projectService                  = projectService,
      globalConfigService             = globalConfigService,
      runtimeVersionManagementService = runtimeVersionManagementService,
      timeoutConfig                   = timeoutConfig
    )
  }

  override def afterEach(): Unit = {
    super.afterEach()
    FileUtils.deleteQuietly(testProjectsRoot)
  }

  val engineToInstall: Option[SemVer] = None

  override def beforeAll(): Unit = {
    super.beforeAll()
    engineToInstall.foreach { version =>
      val os   = OS.operatingSystem.configName
      val ext  = if (OS.isWindows) "zip" else "tar.gz"
      val arch = OS.architecture
      val path = FakeReleases.releaseRoot
        .resolve("enso")
        .resolve(s"enso-$version")
        .resolve(s"enso-engine-$version-$os-$arch.$ext")
        .resolve(s"enso-$version")
        .resolve("component")
      val root = Path.of("../../../").toAbsolutePath.normalize
      println(s"$path --> $root")
      FileUtils.copyFile(
        root.resolve("runner.jar").toFile,
        path.resolve("runner.jar").toFile
      )
      FileUtils.copyFile(
        root.resolve("runtime.jar").toFile,
        path.resolve("runtime.jar").toFile
      )

      val blackhole = system.actorOf(blackholeProps)
      val installAction = runtimeVersionManagementService.installEngine(
        blackhole,
        version,
        forceInstallBroken = false
      )
      Runtime.default.unsafeRun(installAction)
    }
  }
}
