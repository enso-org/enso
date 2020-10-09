package src.main.scala.licenses.backend
import java.nio.file.Path

import src.main.scala.licenses.Attachment

/**
  * An [[AttachmentGatherer]] that combines results from running multiple
  * algorithms.
  */
class CombinedBackend(backends: Seq[AttachmentGatherer])
    extends AttachmentGatherer {

  /**
    * @inheritdoc
    */
  override def run(root: Path): Seq[Attachment] = {
    backends.flatMap(_.run(root))
  }
}

object CombinedBackend {

  /**
    * Creates a [[CombinedBackend]] from multiple [[AttachmentGatherer]]s.
    */
  def apply(backends: AttachmentGatherer*): CombinedBackend =
    new CombinedBackend(backends)
}
