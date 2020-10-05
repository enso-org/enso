package src.main.scala.licenses

import java.nio.file.Path

import sbt.IO

sealed trait Attachment
case class Notice(path: Path, content: String) extends Attachment {
  override def toString: String = s"File: $path"
}
case class CopyrightMention(content: String, context: Option[String])
    extends Attachment {
  override def toString: String = s"Copyright: '$content'"
}

object Notice {
  def read(path: Path, relativizeTo: Option[Path]): Notice = {
    val content = IO.read(path.toFile)
    val actualPath = relativizeTo match {
      case Some(root) => root.relativize(path)
      case None       => path
    }
    Notice(actualPath, content)
  }
}
