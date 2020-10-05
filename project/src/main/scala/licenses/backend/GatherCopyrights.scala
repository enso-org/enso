package src.main.scala.licenses.backend
import java.nio.file.{Files, Path}

import sbt.IO
import src.main.scala.licenses.{Attachment, CopyrightMention}

import scala.util.control.NonFatal

object GatherCopyrights extends AttachmentGatherer {
  override def run(root: Path): Seq[Attachment] = {
    val allCopyrights = AttachmentGatherer.walk(root) { path =>
      if (Files.isRegularFile(path)) {
        try {
          val lines = IO.readLines(path.toFile)
          // TODO [RW] it would be good to add the contexts as they help judge
          //  if the copyright notice is complete
          val relativePath = root.relativize(path)
          lines
            .filter(mayBeCopyright)
            .map(CopyrightMention(_, None, Seq(relativePath)))
        } catch {
          case NonFatal(e) =>
            // TODO maybe gather these ?
            println(s"Ignored file $path, as it cannot be read: $e")
            Seq()
        }
      } else {
        Seq()
      }
    }
    CopyrightMention.merge(allCopyrights)
  }

  private def mayBeCopyright(line: String): Boolean =
    line.toLowerCase.contains("copyright")
}
