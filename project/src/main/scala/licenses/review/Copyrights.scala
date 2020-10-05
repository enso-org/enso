package src.main.scala.licenses.review

import src.main.scala.licenses.{CopyrightMention, Notice}

object Copyrights {
  def processCopyrights(
    copyrights: Seq[CopyrightMention],
    notices: Seq[Notice]
  ): Seq[CopyrightMention] = {
    // TODO add metadata
    copyrights.filter(shouldKeepCopyright(notices))
  }

  def shouldKeepCopyright(
    notices: Seq[Notice]
  )(copyright: CopyrightMention): Boolean = {
    copyright.origins.exists { path =>
      val isAlreadyInSomeNotice = notices.exists(_.path == path)
      !isAlreadyInSomeNotice
    }
  }
}
