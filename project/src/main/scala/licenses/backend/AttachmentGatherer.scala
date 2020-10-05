package src.main.scala.licenses.backend

import java.nio.file.{Files, Path}
import java.util.stream.Collectors

import scala.collection.JavaConverters._
import src.main.scala.licenses.{Attachment, SourceAccess}

trait AttachmentGatherer {
  def run(root: Path): Seq[Attachment]
  def run(sources: Seq[SourceAccess]): Seq[Attachment] = {
    sources.flatMap(_.access(run(_)))
  }
}

object AttachmentGatherer {
  def walk[R](root: Path)(action: Path => Seq[R]): Seq[R] = {
    val stream = Files.walk(root)
    try {
      val list = stream.collect(Collectors.toList())
      list.asScala.flatMap(action)
    } finally {
      stream.close()
    }
  }
}
