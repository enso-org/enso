package src.main.scala.licenses

import java.nio.file.Path

import sbt.IO

sealed trait Attachment
case class AttachedFile(
  path: Path,
  content: String,
  origin: Option[String] = None
) extends Attachment {
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
  def mergeByContent(copyrights: Seq[CopyrightMention]): Seq[CopyrightMention] =
    copyrights
      .groupBy(c => c.content)
      .map({ case (_, equal) => mergeEqual(equal) })
      .toSeq

  def mergeByContext(
    copyrights: Seq[CopyrightMention]
  ): Seq[CopyrightMention] = {
    val (eligible, rest) = copyrights.partition(_.contexts.size == 1)
    val merged = eligible.groupBy(c => c.contexts.head).map {
      case (_, equal) =>
        val ref = findBestRepresentative(equal)
        ref.copy(origins = equal.flatMap(_.origins).distinct)
    }
    (merged ++ rest).toSeq
  }

  private def findBestRepresentative(
    copyrights: Seq[CopyrightMention]
  ): CopyrightMention = {
    copyrights
      .find(_.content.stripLeading.toLowerCase.startsWith("copyright"))
      .getOrElse(copyrights.head)
  }

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

object AttachedFile {
  def read(path: Path, relativizeTo: Option[Path]): AttachedFile = {
    val content = IO.read(path.toFile)
    val actualPath = relativizeTo match {
      case Some(root) => root.relativize(path)
      case None       => path
    }
    AttachedFile(actualPath, content)
  }
}
