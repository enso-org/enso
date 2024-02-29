package org.enso.projectmanager.service.filesystem

import org.apache.commons.io.FileUtils
import org.enso.projectmanager.{BaseServerSpec, ProjectManagementOps}
import org.enso.projectmanager.control.effect.Effects
import org.enso.projectmanager.data.ProjectMetadata
import org.enso.projectmanager.infrastructure.repository.{
  MetadataFileStorage,
  ProjectFileRepositoryFactory
}
import org.enso.runtimeversionmanager.test.OverrideTestVersionSuite
import org.enso.semver.SemVer
import org.scalatest.EitherValues
import zio.{ZAny, ZIO}

import java.io.File
import java.nio.file.Files

class FileSystemServiceSpec
    extends BaseServerSpec
    with ProjectManagementOps
    with OverrideTestVersionSuite
    with Effects
    with EitherValues {

  override val testVersion: SemVer = SemVer.of(0, 0, 1)

  override val engineToInstall: Option[SemVer] = Some(SemVer.of(0, 0, 1))

  lazy val fileSystemService: FileSystemService[ZIO[ZAny, +*, +*]] = {
    val projectRepositoryFactory =
      new ProjectFileRepositoryFactory[ZIO[ZAny, +*, +*]](
        config.storage,
        testClock,
        fileSystem,
        gen
      )
    new FileSystemService[ZIO[ZAny, +*, +*]](
      fileSystem,
      projectRepositoryFactory
    )
  }

  def metadataFileStorage(directory: File) =
    new MetadataFileStorage[ZIO[ZAny, +*, +*]](
      directory,
      config.storage,
      testClock,
      fileSystem,
      gen
    )

  "FileSystemService" should {

    "list directory" in {
      implicit val client: WsTestClient = new WsTestClient(address)

      val testDir = testStorageConfig.userProjectsPath

      val projectName = "New_Project_1"
      val projectId   = createProject(projectName)
      val projectPath = new File(testDir, projectName)

      val testFile = new File(testDir, "foo.txt")

      Files.createFile(testFile.toPath)

      val result = fileSystemService
        .list(testDir)
        .unsafeRunSync()

      result.value should contain theSameElementsAs Seq(
        FileSystemEntry.ProjectEntry(
          projectPath,
          ProjectMetadata(
            projectName,
            "local",
            projectId,
            testClock.currentTime,
            None
          )
        ),
        FileSystemEntry.FileEntry(testFile)
      )

      // cleanup
      deleteProject(projectId)
    }

    "create directory" in {
      val testDir = testStorageConfig.userProjectsPath

      val directoryName = "filesystem_test_create_dir"
      val directoryPath = new File(testDir, directoryName)

      fileSystemService
        .createDirectory(directoryPath)
        .unsafeRunSync()

      Files.isDirectory(directoryPath.toPath) shouldEqual true

      // cleanup
      FileUtils.deleteQuietly(directoryPath)
    }

    "delete directory" in {
      implicit val client: WsTestClient = new WsTestClient(address)

      val testDir = testStorageConfig.userProjectsPath

      val projectName = "New_Project_2"
      createProject(projectName)

      val directoryPath = new File(testDir, projectName)

      fileSystemService
        .deleteDirectory(directoryPath)
        .unsafeRunSync()

      Files.exists(directoryPath.toPath) shouldEqual false
    }

  }
}
