package src.main.scala.licenses.backend
import java.nio.file.Path

import src.main.scala.licenses.Attachment

class CombinedBackend(backends: Seq[AttachmentGatherer])
    extends AttachmentGatherer {
  def run(root: Path): Seq[Attachment] = {
    println(s"Combined $root")
    backends.flatMap(_.run(root))
  }
}

object CombinedBackend {
  def apply(backends: AttachmentGatherer*): CombinedBackend =
    new CombinedBackend(backends)
}
