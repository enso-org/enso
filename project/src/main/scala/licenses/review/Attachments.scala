package src.main.scala.licenses.review

import src.main.scala.licenses.{AttachedFile, Attachment, CopyrightMention}

object Attachments {
  def split(
    attachments: Seq[Attachment]
  ): (Seq[AttachedFile], Seq[CopyrightMention]) = {
    val notices    = attachments.collect { case n: AttachedFile => n }
    val copyrights = attachments.collect { case c: CopyrightMention => c }
    (notices, copyrights)
  }
}
