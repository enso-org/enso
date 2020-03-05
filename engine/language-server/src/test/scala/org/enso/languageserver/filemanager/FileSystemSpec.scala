package org.enso.languageserver.filemanager

import java.nio.file.{Files, Paths}

import cats.effect.IO
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source
import java.nio.file.Path

class FileSystemSpec extends AnyFlatSpec with Matchers {

  "A file system interpreter" should "write textual content to file" in new TestCtx {
    //given
    val path    = Paths.get(testDirPath.toString, "foo.txt")
    val content = "123456789"
    //when
    val result =
      objectUnderTest.write(path.toFile, content).unsafeRunSync()
    //then
    result shouldBe Right(())
    readTxtFile(path) shouldBe content
  }

  it should "overwrite existing files" in new TestCtx {
    //given
    val path            = Paths.get(testDirPath.toString, "foo.txt")
    val existingContent = "123456789"
    val newContent      = "abcdef"
    //when
    objectUnderTest.write(path.toFile, existingContent).unsafeRunSync()
    objectUnderTest.write(path.toFile, newContent).unsafeRunSync()
    //then
    readTxtFile(path) shouldBe newContent
  }

  it should "create the parent directory if it doesn't exist" in new TestCtx {
    //given
    val path    = Paths.get(testDirPath.toString, "foo.txt")
    val content = "123456789"
    testDir.delete()
    //when
    val result =
      objectUnderTest.write(path.toFile, content).unsafeRunSync()
    //then
    result shouldBe Right(())
    readTxtFile(path) shouldBe content
  }

  it should "return FileNotFound failure if the file doesn't exist" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "foo.txt")
    //when
    val result = objectUnderTest.read(path.toFile).unsafeRunSync()
    //then
    result shouldBe Left(FileNotFound)
  }

  it should "read a file content" in new TestCtx {
    //given
    val path    = Paths.get(testDirPath.toString, "foo.txt")
    val content = "123456789"
    objectUnderTest.write(path.toFile, content).unsafeRunSync()
    //when
    val result = objectUnderTest.read(path.toFile).unsafeRunSync()
    //then
    result shouldBe Right(content)
  }

  it should "create a directory" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "foo", "bar")
    //when
    val result = objectUnderTest.createDirectory(path.toFile).unsafeRunSync()
    //then
    result shouldBe Right(())
    path.toFile.isDirectory shouldBe true
  }

  it should "create an empty file" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "foo", "bar", "baz.txt")
    //when
    val result = objectUnderTest.createFile(path.toFile).unsafeRunSync()
    //then
    result shouldBe Right(())
    path.toFile.getParentFile.isDirectory shouldBe true
    path.toFile.isFile shouldBe true
  }

  it should "delete a file" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "foo", "bar.txt")
    createEmptyFile(path)
    path.toFile.isFile shouldBe true
    //when
    val result = objectUnderTest.delete(path.toFile).unsafeRunSync()
    //then
    result shouldBe Right(())
    path.toFile.exists shouldBe false
    path.toFile.getParentFile.isDirectory shouldBe true
  }

  it should "delete a directory recursively" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "foo", "bar.txt")
    createEmptyFile(path)
    path.toFile.isFile shouldBe true
    //when
    val result =
      objectUnderTest.delete(path.toFile.getParentFile).unsafeRunSync()
    //then
    result shouldBe Right(())
    path.toFile.exists shouldBe false
    path.toFile.getParentFile.exists shouldBe false
  }

  it should "return NotFoundError when deleting nonexistent file" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "foo", "bar.txt")
    //when
    val result = objectUnderTest.delete(path.toFile).unsafeRunSync()
    //then
    result shouldBe Left(FileNotFound)
    path.toFile.exists shouldBe false
    path.toFile.getParentFile.exists shouldBe false
  }

  it should "return NotFoundError when deleting nonexistent directory" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "foo", "bar.txt")
    //when
    val result =
      objectUnderTest.delete(path.toFile.getParentFile).unsafeRunSync()
    //then
    result shouldBe Left(FileNotFound)
    path.toFile.exists shouldBe false
    path.toFile.getParentFile.exists shouldBe false
  }

  it should "copy a file" in new TestCtx {
    //given
    val path         = Paths.get(testDirPath.toString, "copy_file", "a.txt")
    val resultCreate = objectUnderTest.createFile(path.toFile).unsafeRunSync()
    resultCreate shouldBe Right(())
    val to = Paths.get(testDirPath.toString, "copy_file", "b.txt")
    //when
    val result = objectUnderTest.copy(path.toFile, to.toFile).unsafeRunSync()
    //then
    result shouldBe Right(())
    path.toFile.isFile shouldBe true
    to.toFile.isFile shouldBe true
  }

  it should "copy a directory" in new TestCtx {
    //given
    val path         = Paths.get(testDirPath.toString, "copy_dir", "a.txt")
    val resultCreate = objectUnderTest.createFile(path.toFile).unsafeRunSync()
    resultCreate shouldBe Right(())
    val from = path.getParent()
    val to   = Paths.get(testDirPath.toString, "copy_dir", "to")
    //when
    val result = objectUnderTest.copy(from.toFile, to.toFile).unsafeRunSync()
    //then
    result shouldBe Right(())
    path.toFile.isFile shouldBe true
    to.toFile.isDirectory shouldBe true
    to.resolve(path.getFileName).toFile.isFile shouldBe true
  }

  it should "copy a file to existing directory" in new TestCtx {
    //given
    val from = Paths.get(testDirPath.toString, "copy_dir", "a.txt")
    val resultCreateFile =
      objectUnderTest.createFile(from.toFile).unsafeRunSync()
    resultCreateFile shouldBe Right(())
    val to = Paths.get(testDirPath.toString, "copy_dir", "to")
    val resultCreateDirectory =
      objectUnderTest.createDirectory(to.toFile).unsafeRunSync()
    resultCreateDirectory shouldBe Right(())
    to.toFile.isDirectory shouldBe true
    //when
    val result = objectUnderTest.copy(from.toFile, to.toFile).unsafeRunSync()
    //then
    result shouldBe Right(())
    from.toFile.isFile shouldBe true
    to.resolve(from.getFileName).toFile.isFile shouldBe true
  }

  it should "return FileExists error when copying directory to existing file" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "copy_dir", "a.txt")
    val resultCreateFile =
      objectUnderTest.createFile(path.toFile).unsafeRunSync()
    resultCreateFile shouldBe Right(())
    val from = path.getParent()
    val to   = Paths.get(testDirPath.toString, "copy_to", "b.txt")
    val resultCreateDirectory =
      objectUnderTest.createFile(to.toFile).unsafeRunSync()
    resultCreateDirectory shouldBe Right(())
    //when
    val result = objectUnderTest.copy(from.toFile, to.toFile).unsafeRunSync()
    //then
    result shouldBe Left(FileExists)
    path.toFile.isFile shouldBe true
    to.toFile.isFile shouldBe true
  }

  it should "return FileNotFound when copy nonexistent file" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "copy_nonexistent", "a.txt")
    val to   = Paths.get(testDirPath.toString, "copy_file", "b.txt")
    path.toFile.isFile shouldBe false
    //when
    val result = objectUnderTest.copy(path.toFile, to.toFile).unsafeRunSync()
    //then
    result shouldBe Left(FileNotFound)
    path.toFile.exists shouldBe false
    to.toFile.exists shouldBe false
  }

  it should "reutrn FileNotFound when copy nonexistent directory" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "copy_nonexistent")
    val to   = Paths.get(testDirPath.toString, "copy_file")
    path.toFile.exists shouldBe false
    //when
    val result = objectUnderTest.copy(path.toFile, to.toFile).unsafeRunSync()
    //then
    result shouldBe Left(FileNotFound)
    path.toFile.exists shouldBe false
    to.toFile.exists shouldBe false
  }

  def readTxtFile(path: Path): String = {
    val buffer  = Source.fromFile(path.toFile)
    val content = buffer.getLines().mkString
    buffer.close()
    content
  }

  def createEmptyFile(path: Path): Path = {
    Files.createDirectories(path.getParent())
    Files.createFile(path)
  }

  trait TestCtx {

    val testDirPath = Files.createTempDirectory(null)

    val testDir = testDirPath.toFile
    testDir.deleteOnExit()

    val objectUnderTest = new FileSystem[IO]

  }

}
