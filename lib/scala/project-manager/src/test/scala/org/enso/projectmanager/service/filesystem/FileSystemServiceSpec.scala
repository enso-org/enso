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

import java.io.{ByteArrayInputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.attribute.BasicFileAttributes

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

  def attributes(path: File): Attributes =
    Attributes(Files.readAttributes(path.toPath, classOf[BasicFileAttributes]))

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
          attributes(projectPath),
          ProjectMetadata(
            projectName,
            "local",
            projectId,
            testClock.currentTime,
            None
          )
        ),
        FileSystemEntry.FileEntry(testFile, attributes(testFile))
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
        .delete(directoryPath)
        .unsafeRunSync()

      Files.exists(directoryPath.toPath) shouldEqual false
    }

    "delete file" in {
      val testDir  = testStorageConfig.userProjectsPath
      val testFile = new File(testDir, "foo.txt")
      FileUtils.forceMkdir(testDir)
      Files.createFile(testFile.toPath)

      fileSystemService
        .delete(testFile)
        .unsafeRunSync()

      Files.exists(testFile.toPath) shouldEqual false
    }

    "move file" in {
      val testDir = testStorageConfig.userProjectsPath

      val targetFileName      = "target_move_file.txt"
      val destinationFileName = "destination_move_file.txt"
      val targetFilePath      = new File(testDir, targetFileName)
      val destinationFilePath = new File(testDir, destinationFileName)

      FileUtils.forceMkdirParent(targetFilePath)
      FileUtils.touch(targetFilePath)

      fileSystemService
        .move(targetFilePath, destinationFilePath)
        .unsafeRunSync()

      Files.exists(targetFilePath.toPath) shouldEqual false
      Files.exists(destinationFilePath.toPath) shouldEqual true

      // cleanup
      FileUtils.deleteQuietly(destinationFilePath)
    }

    "move directory" in {
      implicit val client: WsTestClient = new WsTestClient(address)

      val testDir = testStorageConfig.userProjectsPath

      val projectName = "New_Project_3"
      createProject(projectName)

      val directoryPath = new File(testDir, projectName)
      val targetPath    = new File(testDir, "Target_Move_Directory")

      fileSystemService
        .move(directoryPath, targetPath)
        .unsafeRunSync()

      Files.exists(directoryPath.toPath) shouldEqual false
      Files.isDirectory(targetPath.toPath) shouldEqual true

      // cleanup
      FileUtils.deleteQuietly(targetPath)
    }

    "write path" in {
      val testDir = testStorageConfig.userProjectsPath

      val fileName = "filesystem_test_write_path.txt"
      val filePath = new File(testDir, fileName)
      val contents = "Hello World!"

      fileSystemService
        .write(
          filePath,
          new ByteArrayInputStream(contents.getBytes(StandardCharsets.UTF_8))
        )
        .unsafeRunSync()

      val bytes = Files.readAllBytes(filePath.toPath)
      new String(bytes, StandardCharsets.UTF_8) shouldEqual contents

      // cleanup
      FileUtils.deleteQuietly(filePath)
    }

  }
}
