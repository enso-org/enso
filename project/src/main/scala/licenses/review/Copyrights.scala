package src.main.scala.licenses.review

import src.main.scala.licenses.{AttachedFile, CopyrightMention}

object Copyrights {
  def removeCopyrightsIncludedInNotices(
    copyrights: Seq[CopyrightMention],
    notices: Seq[AttachedFile]
  ): Seq[CopyrightMention] = {
    copyrights.filter(shouldKeepCopyright(notices))
  }

  def shouldKeepCopyright(
    notices: Seq[AttachedFile]
  )(copyright: CopyrightMention): Boolean = {
    val allOriginsAreFresh = copyright.origins.forall { path =>
      !notices.exists(_.path == path)
    }
    allOriginsAreFresh
  }
}
