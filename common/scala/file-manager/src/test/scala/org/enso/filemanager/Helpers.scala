package org.enso.filemanager

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

import org.apache.commons.io.FileUtils

import org.scalatest.Matchers

trait Helpers extends Matchers {
  var tempDir: Path = _

  val contents: Array[Byte] =
    "葦垣の中の和草にこやかに我れと笑まして人に知らゆな\nzażółć gęślą jaźń".getBytes

  def createSubFile(): Path = {
    val path = Files.createTempFile(tempDir, "foo", "")
    Files.write(path, contents)
  }

  def createSubDir(): Path = {
    Files.createTempDirectory(tempDir, "foo")
  }

  def homeDirectory(): Path = Paths.get(System.getProperty("user.home"))

  def setupTemp(): Unit = {
    tempDir = Files.createTempDirectory("file-manager-test")
  }

  def cleanTemp(): Unit = {
    FileUtils.deleteDirectory(tempDir.toFile)
    tempDir = null
  }

  def withTemporaryDirectory[ret](f: Path => ret): ret = {
    setupTemp()
    try f(tempDir)
    finally cleanTemp()
  }

  case class Subtree(
    root: Path,
    childrenFiles: Seq[Path],
    childrenDirs: Seq[Path]) {

    val elements: Seq[Path] =
      (Seq(root) ++ childrenDirs ++ childrenFiles).map(root.resolve)

    def rebase(otherRoot: Path): Subtree =
      Subtree(otherRoot, childrenFiles, childrenDirs)
  }

  def createSubtree(): Subtree = {
    val root       = createSubDir()
    val rootFile1  = Paths.get("file1")
    val rootSubDir = Paths.get("dir")
    val rootFile2  = Paths.get("dir/file2")

    Files.write(root.resolve(rootFile1), contents)
    Files.createDirectory(root.resolve(rootSubDir))
    Files.write(root.resolve(rootFile2), contents)
    Subtree(root, Seq(rootFile1, rootFile2), Seq(rootSubDir))
  }

  def expectSubtree(subtree: Subtree): Unit = {
    assert(Files.exists(subtree.root))
    subtree.elements.foreach(
      elem => expectExist(subtree.root.resolve(elem))
    )

    val listStream = Files.list(subtree.root)
    try listStream.count() should be(2)
    finally listStream.close()
  }

  def expectExist(path: Path): Unit = {
    assert(Files.exists(path), s"$path is expected to exist")
  }

  def expectNotExist(path: Path): Unit = {
    assert(!Files.exists(path), s"$path is expected to not exist")
  }
}
