package src.main.scala.licenses.review

import src.main.scala.licenses.{Attachment, CopyrightMention, Notice}

object Attachments {
  def processAttachments(attachments: Seq[Attachment]): Seq[Attachment] = {
    // TODO add metadata

    val notices    = attachments.collect { case n: Notice => n }
    val copyrights = attachments.collect { case c: CopyrightMention => c }

    val processedCopyrights = Copyrights.processCopyrights(copyrights, notices)
    val processedNotices    = notices // TODO
    processedCopyrights ++ processedNotices
  }
}
