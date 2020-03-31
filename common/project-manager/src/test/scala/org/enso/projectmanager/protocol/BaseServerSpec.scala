package org.enso.projectmanager.protocol

import java.io.File
import java.nio.file.Files
import java.time.{OffsetDateTime, ZoneOffset}
import java.util.UUID

import io.circe.generic.auto._
import org.enso.jsonrpc.test.JsonRpcServerTestKit
import org.enso.jsonrpc.{ClientControllerFactory, Protocol}
import org.enso.projectmanager.control.effect.ZioEnvExec
import org.enso.projectmanager.infrastructure.file.{
  BlockingFileSystem,
  SynchronizedFileStorage
}
import org.enso.projectmanager.infrastructure.languageserver.{
  LanguageServerRegistry,
  LanguageServerRegistryProxy,
  LanguageServerService
}
import org.enso.projectmanager.infrastructure.repository.{
  ProjectFileRepository,
  ProjectIndex
}
import org.enso.projectmanager.boot.configuration.{
  BootloaderConfig,
  NetworkConfig,
  StorageConfig,
  TimeoutConfig
}
import org.enso.projectmanager.service.{MonadicProjectValidator, ProjectService}
import org.enso.projectmanager.test.{ConstGenerator, NopLogging, StoppedClock}
import zio.interop.catz.core._
import zio.{Runtime, Semaphore, ZEnv, ZIO}

import scala.concurrent.duration._

class BaseServerSpec extends JsonRpcServerTestKit {

  override def protocol: Protocol = JsonRpc.protocol

  val TestNow = OffsetDateTime.now(ZoneOffset.UTC)

  val testClock = new StoppedClock[ZEnv](TestNow)

  val TestUUID = UUID.randomUUID()

  lazy val gen = new ConstGenerator[ZEnv](TestUUID)

  val testProjectsRoot = Files.createTempDirectory(null).toFile
  testProjectsRoot.deleteOnExit()

  val userProjectDir = new File(testProjectsRoot, "projects")

  val indexFile = new File(testProjectsRoot, "project-index.json")

  lazy val testStorageConfig = StorageConfig(
    projectsRoot        = testProjectsRoot,
    projectMetadataPath = indexFile,
    userProjectsPath    = userProjectDir
  )

  lazy val bootloaderConfig = BootloaderConfig(3, 1.second)

  lazy val timeoutConfig = TimeoutConfig(3.seconds, 3.seconds, 3.seconds)

  lazy val netConfig = NetworkConfig("127.0.0.1", 40000, 60000)

  implicit val exec = new ZioEnvExec(Runtime.default)

  lazy val fileSystem = new BlockingFileSystem(5.seconds)

  lazy val storageSemaphore =
    Runtime.default.unsafeRun(Semaphore.make(1))

  lazy val indexStorage =
    new SynchronizedFileStorage[ProjectIndex, ZIO[ZEnv, +*, +*]](
      testStorageConfig.projectMetadataPath,
      fileSystem
    )

  lazy val projectRepository =
    new ProjectFileRepository(
      testStorageConfig,
      fileSystem,
      indexStorage
    )

  lazy val projectValidator = new MonadicProjectValidator[ZIO[ZEnv, *, *]]()

  lazy val languageServerRegistry =
    system.actorOf(LanguageServerRegistry.props(netConfig, bootloaderConfig))

  lazy val languageServerService =
    new LanguageServerRegistryProxy[ZIO[ZEnv, +*, +*]](
      languageServerRegistry,
      timeoutConfig
    )

  lazy val projectService =
    new ProjectService[ZIO[ZEnv, +*, +*]](
      projectValidator,
      projectRepository,
      new NopLogging[ZEnv],
      testClock,
      gen,
      languageServerService
    )

  override def clientControllerFactory: ClientControllerFactory = {
    new ManagerClientControllerFactory[ZIO[ZEnv, +*, +*]](
      system,
      projectService,
      timeoutConfig
    )
  }

}
