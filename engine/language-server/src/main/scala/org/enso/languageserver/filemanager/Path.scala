package org.enso.languageserver.filemanager

import java.io.File
import java.nio
import java.util.UUID

/** A representation of a path relative to a specified content root.
  *
  * @param rootId a content root id that the path is relative to
  * @param segments path segments
  */
case class Path(rootId: UUID, segments: Vector[String]) {

  /** Given the filesystem path of the content root, resolves this path relative
    * to the content root path.
    */
  def toFile(rootPath: File): File =
    segments.foldLeft(rootPath) { case (parent, child) =>
      new File(parent, child)
    }

  /** Given the filesystem path of the content root and a filename, treats the
    *  current path as a directory path and resolves a path to the provided file
    *  inside of the directory indicated by this path, relative to the content
    *  root path.
    */
  def toFileInsideThisDirectory(rootPath: File, fileName: String): File = {
    val parentDir = toFile(rootPath)
    new File(parentDir, fileName)
  }

  /** Checks if the provided path is a prefix of the current path.
    * The content root of the provided path has to be identical to the content root of the current one.
    */
  def startsWith(path: Path): Boolean = {
    (rootId == path.rootId) && (segments.length >= path.segments.length) && segments
      .zip(path.segments)
      .forall({ case (s1, s2) => s1 == s2 })
  }
}

object Path {

  def apply(rootId: UUID, path: nio.file.Path): Path =
    new Path(rootId, Path.segments(path))

  def segments(path: nio.file.Path): Vector[String] = {
    val b = Vector.newBuilder[String]
    path.forEach(p => b += p.toString())
    b.result().filter(_.nonEmpty)
  }

  /** Get path relative to the root.
    *
    * @param root a root path
    * @param base a path relative to the root
    * @param path a path that will be relativized
    * @return a path relative to the root
    */
  def getRelativePath(root: File, base: Path, path: nio.file.Path): Path =
    Path(base.rootId, root.toPath.relativize(path))

  /** Get path relative to the root, and return a parent path.
    *
    * @param root a root path
    * @param base a path relative to the root
    * @param path a path that will be relativized
    * @return a parent of a path relative to the root
    */
  def getRelativeParent(root: File, base: Path, path: nio.file.Path): Path =
    getRelativePath(root, base, path.getParent())
}
