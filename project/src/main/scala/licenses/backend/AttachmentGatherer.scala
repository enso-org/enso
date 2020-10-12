package src.main.scala.licenses.backend

import java.nio.file.{Files, Path}
import java.util.stream.Collectors

import src.main.scala.licenses.{Attachment, SourceAccess}

import scala.collection.JavaConverters._

/**
  * Common interface for algorithms that gather attachments based on available
  * sources.
  */
trait AttachmentGatherer {

  /**
    * Runs the algorithm that will scan the sources located in `root` and return
    * any attachments found.
    */
  def run(root: Path): Seq[Attachment]

  /**
    * A helper function that uses the provided [[SourceAccess]] instances to
    * analyze sources provided by them and return any attachments found.
    */
  def run(sources: Seq[SourceAccess]): Seq[Attachment] = {
    sources.flatMap(_.access(run(_)))
  }
}

object AttachmentGatherer {

  /**
    * A helper method that recursively traverses the directory structure at
    * `root` and collects results of calling `action` on each encountered entry.
    *
    * The action is called for all kinds of entries that are encountered.
    */
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
