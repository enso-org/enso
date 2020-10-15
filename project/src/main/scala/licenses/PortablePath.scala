package src.main.scala.licenses

import java.io.{File => JFile}
import java.nio.file.Path

import sbt.File

/**
  * A wrapper for [[Path]] that ensures portable string representation on all
  * platforms.
  */
case class PortablePath(path: Path) {

  /**
    * @inheritdoc
    */
  override def toString: String =
    path.toString.replace(JFile.separatorChar, '/')

  /**
    * A utility method converting the wrapped path to [[File]].
    */
  def toFile: File = path.toFile

  /**
    * A utility method returning filename of the wrapped path.
    */
  def getFileName: String = path.getFileName.toString
}

object PortablePath {

  /**
    * A utility method that parses [[Path]] and immediately wraps it into
    * [[PortablePath]].
    */
  def of(first: String, more: String*): PortablePath =
    PortablePath(Path.of(first, more: _*))
}
