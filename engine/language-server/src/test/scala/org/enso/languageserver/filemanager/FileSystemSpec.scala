package org.enso.languageserver.filemanager

import java.nio.file.{Files, Path, Paths}
import java.nio.file.attribute.BasicFileAttributes

import org.apache.commons.io.FileUtils
import org.enso.languageserver.effect.Effects
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

class FileSystemSpec extends AnyFlatSpec with Matchers with Effects {

  import FileSystemApi._

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

  it should "write binary contents to a file" in new TestCtx {
    //given
    val path    = Paths.get(testDirPath.toString, "foo.txt")
    val content = Array[Byte](1, 2, 3)
    //when
    val result =
      objectUnderTest.writeBinary(path.toFile, content).unsafeRunSync()
    val Right(savedContent) =
      objectUnderTest.readBinary(path.toFile).unsafeRunSync()
    //then
    result shouldBe Right(())
    savedContent.toList shouldBe content.toList
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

  it should "overwrite existing binary files" in new TestCtx {
    //given
    val path            = Paths.get(testDirPath.toString, "foo.txt")
    val existingContent = Array[Byte](1, 2, 3)
    val newContent      = Array[Byte](3, 1, 2)
    //when
    objectUnderTest.writeBinary(path.toFile, existingContent).unsafeRunSync()
    objectUnderTest.writeBinary(path.toFile, newContent).unsafeRunSync()
    val Right(savedContent) =
      objectUnderTest.readBinary(path.toFile).unsafeRunSync()
    //then
    savedContent.toList shouldBe newContent.toList
  }

  it should "create the parent directory if it doesn't exist" in new TestCtx {
    //given
    val path    = Paths.get(testDirPath.toString, "foo.txt")
    val content = "123456789"
    testDirPath.toFile.delete()
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

  it should "move a file" in new TestCtx {
    //given
    val path         = Paths.get(testDirPath.toString, "move_file", "a.txt")
    val resultCreate = objectUnderTest.createFile(path.toFile).unsafeRunSync()
    resultCreate shouldBe Right(())
    val to = Paths.get(testDirPath.toString, "move_file", "b.txt")
    //when
    val result = objectUnderTest.move(path.toFile, to.toFile).unsafeRunSync()
    //then
    result shouldBe Right(())
    path.toFile.exists shouldBe false
    to.toFile.isFile shouldBe true
  }

  it should "move a directory" in new TestCtx {
    //given
    val path         = Paths.get(testDirPath.toString, "move_dir", "a.txt")
    val resultCreate = objectUnderTest.createFile(path.toFile).unsafeRunSync()
    resultCreate shouldBe Right(())
    val from = path.getParent()
    val to   = Paths.get(testDirPath.toString, "move_dir_to")
    //when
    val result = objectUnderTest.move(from.toFile, to.toFile).unsafeRunSync()
    //then
    result shouldBe Right(())
    from.toFile.exists shouldBe false
    to.toFile.isDirectory shouldBe true
    to.resolve(path.getFileName()).toFile.isFile shouldBe true
  }

  it should "move a file to existing directory" in new TestCtx {
    //given
    val from = Paths.get(testDirPath.toString, "move_dir", "a.txt")
    val resultCreateFile =
      objectUnderTest.createFile(from.toFile).unsafeRunSync()
    resultCreateFile shouldBe Right(())
    val to = Paths.get(testDirPath.toString, "move_to")
    val resultCreateDirectory =
      objectUnderTest.createDirectory(to.toFile).unsafeRunSync()
    resultCreateDirectory shouldBe Right(())
    //when
    val result = objectUnderTest.move(from.toFile, to.toFile).unsafeRunSync()
    //then
    result shouldBe Right(())
    from.toFile.exists shouldBe false
    to.resolve(from.getFileName).toFile.isFile shouldBe true
  }

  it should "move a directory to existing directory" in new TestCtx {
    //given
    val from = Paths.get(testDirPath.toString, "move_dir", "a.txt")
    val resultCreateFile =
      objectUnderTest.createFile(from.toFile).unsafeRunSync()
    resultCreateFile shouldBe Right(())
    val to = Paths.get(testDirPath.toString, "move_to")
    val resultCreateDirectory =
      objectUnderTest.createDirectory(to.toFile).unsafeRunSync()
    resultCreateDirectory shouldBe Right(())
    //when
    val result =
      objectUnderTest.move(from.getParent.toFile, to.toFile).unsafeRunSync()
    //then
    val dest = Paths.get(testDirPath.toString, "move_to", "move_dir")
    result shouldBe Right(())
    from.toFile.exists shouldBe false
    from.getParent.toFile.exists shouldBe false
    dest.toFile.isDirectory shouldBe true
    dest.resolve(from.getFileName).toFile.isFile shouldBe true
  }

  it should "return FileNotFound when moving nonexistent file" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "nonexistent", "a.txt")
    val to   = Paths.get(testDirPath.toString, "move_file", "b.txt")
    //when
    val result = objectUnderTest.move(path.toFile, to.toFile).unsafeRunSync()
    //then
    result shouldBe Left(FileNotFound)
    path.toFile.exists shouldBe false
    to.toFile.exists shouldBe false
  }

  it should "return FileExists when moving to existing destination" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "move_file", "a.txt")
    val resultCreateFrom =
      objectUnderTest.createFile(path.toFile).unsafeRunSync()
    resultCreateFrom shouldBe Right(())
    val to             = Paths.get(testDirPath.toString, "move_file", "b.txt")
    val resultCreateTo = objectUnderTest.createFile(to.toFile).unsafeRunSync()
    resultCreateTo shouldBe Right(())
    //when
    val result = objectUnderTest.move(path.toFile, to.toFile).unsafeRunSync()
    //then
    result shouldBe Left(FileExists)
    path.toFile.isFile shouldBe true
    to.toFile.isFile shouldBe true
  }

  it should "check file existence" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "foo", "bar.txt")
    createEmptyFile(path)
    path.toFile.isFile shouldBe true
    //when
    val result = objectUnderTest.exists(path.toFile).unsafeRunSync()
    //then
    result shouldBe Right(true)
  }

  it should "check file non-existence" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "nonexistent.txt")
    path.toFile.exists shouldBe false
    //when
    val result = objectUnderTest.exists(path.toFile).unsafeRunSync()
    //then
    result shouldBe Right(false)
  }

  it should "list directory contents" in new TestCtx {
    //given
    val path    = Paths.get(testDirPath.toString, "list")
    val fileA   = Paths.get(testDirPath.toString, "list", "a.txt")
    val symlink = Paths.get(testDirPath.toString, "list", "b.symlink")
    val subdir  = Paths.get(testDirPath.toString, "list", "subdir")
    val fileB   = Paths.get(testDirPath.toString, "list", "subdir", "b.txt")
    createEmptyFile(fileA)
    createEmptyFile(fileB)
    Files.createSymbolicLink(symlink, fileA)
    //when
    val result = objectUnderTest.list(path.toFile).unsafeRunSync()
    //then
    result shouldBe Right(
      Vector(
        FileEntry(fileA),
        FileEntry(symlink),
        DirectoryEntryTruncated(subdir)
      )
    )
  }

  it should "return FileNotFound error when listing nonexistent path" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "nonexistent")
    //when
    val result = objectUnderTest.list(path.toFile).unsafeRunSync()
    //then
    result shouldBe Left(FileNotFound)
  }

  it should "return NotDirectory error when listing a file" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "a.txt")
    createEmptyFile(path)
    //when
    val result = objectUnderTest.list(path.toFile).unsafeRunSync()
    //then
    result shouldBe Left(NotDirectory)
  }

  it should "tree directory contents" in new TestCtx {
    //given
    val path     = Paths.get(testDirPath.toString, "dir")
    val subdir   = Paths.get(testDirPath.toString, "dir", "subdir")
    val fileA    = Paths.get(testDirPath.toString, "dir", "subdir", "a.txt")
    val fileB    = Paths.get(testDirPath.toString, "dir", "subdir", "b.txt")
    val symlink  = Paths.get(testDirPath.toString, "dir", "symlink")
    val symFileA = Paths.get(testDirPath.toString, "dir", "symlink", "a.txt")
    val symFileB = Paths.get(testDirPath.toString, "dir", "symlink", "b.txt")
    createEmptyFile(fileA)
    createEmptyFile(fileB)
    Files.createSymbolicLink(symlink, subdir)
    val expectedEntry = DirectoryEntry(
      path,
      ArrayBuffer(
        DirectoryEntry(
          subdir,
          ArrayBuffer(
            FileEntry(fileA),
            FileEntry(fileB)
          )
        ),
        DirectoryEntry(
          symlink,
          ArrayBuffer(
            FileEntry(symFileA),
            FileEntry(symFileB)
          )
        )
      )
    )
    //when
    val result = objectUnderTest.tree(path.toFile, depth = None).unsafeRunSync()
    //then
    result shouldBe Right(expectedEntry)
  }

  it should "tree directory and limit depth" in new TestCtx {
    //given
    val path    = Paths.get(testDirPath.toString, "dir")
    val subdir  = Paths.get(testDirPath.toString, "dir", "subdir")
    val fileA   = Paths.get(testDirPath.toString, "dir", "subdir", "a.txt")
    val fileB   = Paths.get(testDirPath.toString, "dir", "subdir", "b.txt")
    val symlink = Paths.get(testDirPath.toString, "dir", "symlink")
    createEmptyFile(fileA)
    createEmptyFile(fileB)
    Files.createSymbolicLink(symlink, subdir)
    val expectedEntry = DirectoryEntry(
      path,
      ArrayBuffer(
        DirectoryEntryTruncated(subdir),
        DirectoryEntryTruncated(symlink)
      )
    )
    //when
    val result =
      objectUnderTest.tree(path.toFile, depth = Some(1)).unsafeRunSync()
    //then
    result shouldBe Right(expectedEntry)
  }

  it should "tree directory and detect symlink loops" in new TestCtx {
    //given
    val path     = Paths.get(testDirPath.toString, "dir")
    val dirA     = Paths.get(testDirPath.toString, "dir", "a")
    val symlinkB = Paths.get(testDirPath.toString, "dir", "a", "symlink_b")
    val dirB     = Paths.get(testDirPath.toString, "dir", "b")
    val symlinkA = Paths.get(testDirPath.toString, "dir", "b", "symlink_a")
    Files.createDirectories(dirA)
    Files.createDirectories(dirB)
    Files.createSymbolicLink(symlinkB, dirB)
    Files.createSymbolicLink(symlinkA, dirA)

    val expectedEntry =
      DirectoryEntry(
        path,
        ArrayBuffer(
          DirectoryEntry(
            dirA,
            ArrayBuffer(
              DirectoryEntry(
                symlinkB,
                ArrayBuffer(
                  DirectoryEntry(
                    Paths.get(symlinkB.toString, "symlink_a"),
                    ArrayBuffer(
                      SymbolicLinkEntry(
                        Paths.get(symlinkB.toString, "symlink_a", "symlink_b"),
                        symlinkB
                      )
                    )
                  )
                )
              )
            )
          ),
          DirectoryEntry(
            dirB,
            ArrayBuffer(
              DirectoryEntry(
                symlinkA,
                ArrayBuffer(
                  DirectoryEntry(
                    Paths.get(symlinkA.toString, "symlink_b"),
                    ArrayBuffer(
                      SymbolicLinkEntry(
                        Paths.get(symlinkA.toString, "symlink_b", "symlink_a"),
                        symlinkA
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    //when
    val result = objectUnderTest.tree(path.toFile, depth = None).unsafeRunSync()
    //then
    result shouldBe Right(expectedEntry)
  }

  it should "tree directory with broken symlinks" in new TestCtx {
    //given
    val path    = Paths.get(testDirPath.toString, "dir")
    val fileA   = Paths.get(testDirPath.toString, "dir", "a.txt")
    val symlink = Paths.get(testDirPath.toString, "dir", "symlink")
    Files.createDirectories(path)
    Files.createSymbolicLink(symlink, fileA)
    val expectedEntry = DirectoryEntry(
      path,
      ArrayBuffer(
        OtherEntry(symlink)
      )
    )
    //when
    val result = objectUnderTest.tree(path.toFile, depth = None).unsafeRunSync()
    //then
    result shouldBe Right(expectedEntry)
  }

  it should "return NotDirectory when tree path is not a directory" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "dir", "a.txt")
    createEmptyFile(path)
    //when
    val result = objectUnderTest.tree(path.toFile, depth = None).unsafeRunSync()
    //then
    result shouldBe Left(NotDirectory)
  }

  it should "return FileNotFound when tree depth <= 0" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "dir", "a.txt")
    createEmptyFile(path)
    //when
    val result = objectUnderTest
      .tree(path.getParent.toFile, depth = Some(0))
      .unsafeRunSync()
    //then
    result shouldBe Left(FileNotFound)
  }

  it should "get attributes of a file" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "a.txt")
    createEmptyFile(path)
    val attrs = Files.readAttributes(path, classOf[BasicFileAttributes])
    //when
    val result = objectUnderTest.info(path.toFile).unsafeRunSync()
    //then
    val expectedAttrs = Attributes(
      creationTime     = attrs.creationTime,
      lastAccessTime   = attrs.lastAccessTime,
      lastModifiedTime = attrs.lastModifiedTime,
      kind             = FileEntry(path),
      byteSize         = 0
    )
    result shouldBe Right(expectedAttrs)
  }

  it should "get attributes of a directory" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "dir", "a.txt")
    val dir  = path.getParent()
    createEmptyFile(path)
    //when
    val result = objectUnderTest.info(dir.toFile).unsafeRunSync()
    //then
    result.map(_.kind) shouldBe Right(DirectoryEntryTruncated(dir))
  }

  it should "get attributes of a symlink" in new TestCtx {
    //given
    val path    = Paths.get(testDirPath.toString, "a.txt")
    val symlink = Paths.get(testDirPath.toString, "symlink.txt")
    createEmptyFile(path)
    Files.createSymbolicLink(symlink, path)
    //when
    val result = objectUnderTest.info(symlink.toFile).unsafeRunSync()
    //then
    result.map(_.kind) shouldBe Right(FileEntry(symlink))
  }

  it should "return FileNotFound getting attributes if file does not exist" in new TestCtx {
    //given
    val path = Paths.get(testDirPath.toString, "nonexistent.txt")
    //when
    val result = objectUnderTest.info(path.toFile).unsafeRunSync()
    //then
    result shouldBe Left(FileNotFound)
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
    sys.addShutdownHook(FileUtils.deleteQuietly(testDirPath.toFile))

    val objectUnderTest = new FileSystem

  }

}
