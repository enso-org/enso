package org.enso.projectmanager

import java.io.File
import java.nio.file.Files
import java.time.{OffsetDateTime, ZoneOffset}
import java.util.UUID

import akka.testkit._
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
import org.enso.projectmanager.service.{MonadicProjectValidator, ProjectService}
import org.enso.projectmanager.test.{ObservableGenerator, ProgrammableClock}
import pureconfig.ConfigSource
import pureconfig.generic.auto._
import zio.interop.catz.core._
import zio.{Runtime, Semaphore, ZEnv, ZIO}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class BaseServerSpec extends JsonRpcServerTestKit {

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

  lazy val projectService =
    new ProjectService[ZIO[ZEnv, +*, +*]](
      projectValidator,
      projectRepository,
      new Slf4jLogging[ZIO[ZEnv, +*, +*]],
      testClock,
      gen,
      languageServerGateway
    )

  override def clientControllerFactory: ClientControllerFactory = {
    new ManagerClientControllerFactory[ZIO[ZEnv, +*, +*]](
      system,
      projectService,
      timeoutConfig
    )
  }

  override def afterEach(): Unit = {
    super.afterEach()
    FileUtils.deleteQuietly(testProjectsRoot)
  }

}
