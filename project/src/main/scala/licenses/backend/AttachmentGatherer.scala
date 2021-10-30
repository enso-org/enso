package src.main.scala.licenses.backend

import java.nio.file.Path

import src.main.scala.licenses.{Attachment, SourceAccess}

/** Common interface for algorithms that gather attachments based on available
  * sources.
  */
trait AttachmentGatherer {

  /** Runs the algorithm that will scan the sources located in `root` and return
    * any attachments found.
    */
  def run(root: Path): Seq[Attachment]

  /** A helper function that uses the provided [[SourceAccess]] instances to
    * analyze sources provided by them and return any attachments found.
    */
  def run(sources: Seq[SourceAccess]): Seq[Attachment] = {
    sources.flatMap(_.access(run(_)))
  }
}
