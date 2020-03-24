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
import org.enso.projectmanager.infrastructure.repository.{
  ProjectFileRepository,
  ProjectIndex
}
import org.enso.projectmanager.main.configuration.StorageConfig
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

  lazy val projectService =
    new ProjectService[ZIO[ZEnv, +*, +*]](
      projectValidator,
      projectRepository,
      new NopLogging[ZEnv],
      testClock,
      gen
    )

  override def clientControllerFactory: ClientControllerFactory = {
    new ManagerClientControllerFactory[ZIO[ZEnv, +*, +*]](
      system,
      projectService,
      10.seconds
    )
  }

}
