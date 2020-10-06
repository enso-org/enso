package src.main.scala.licenses

import java.nio.file.Path

import sbt.IO

sealed trait Attachment
case class Notice(path: Path, content: String) extends Attachment {
  override def toString: String = s"File: $path"

  def fileName: String = path.getFileName.toString
}
case class CopyrightMention(
  content: String,
  contexts: Seq[String],
  origins: Seq[Path]
) extends Attachment {
  override def toString: String = s"Copyright: '$content'"
}

object CopyrightMention {
  def merge(copyrights: Seq[CopyrightMention]): Seq[CopyrightMention] =
    copyrights
      .groupBy(c => c.content)
      .map({ case (_, equal) => mergeEqual(equal) })
      .toSeq

  def mergeEqual(copyrights: Seq[CopyrightMention]): CopyrightMention = {
    val ref = copyrights.headOption.getOrElse(
      throw new IllegalArgumentException("Copyrights must not be empty")
    )
    val ok = copyrights.forall(_.content == ref.content)
    if (!ok) {
      throw new IllegalArgumentException("Copyrights must be equal to merge")
    }
    CopyrightMention(
      ref.content,
      copyrights.flatMap(_.contexts).distinct,
      copyrights.flatMap(_.origins)
    )
  }
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
