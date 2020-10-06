package src.main.scala.licenses

import java.nio.file.Path

case class DependencySummary(
  dependencies: Seq[(DependencyInformation, Seq[Attachment])]
)

sealed trait AttachmentStatus {
  def included: Boolean
}
object AttachmentStatus {
  case object Keep extends AttachmentStatus {
    override def included: Boolean = true
  }
  case object KeepWithContext extends AttachmentStatus {
    override def included: Boolean = true
  }
  case object Ignore extends AttachmentStatus {
    override def included: Boolean = false
  }
  case object Added extends AttachmentStatus {
    override def included: Boolean = true
  }
  case object NotReviewed extends AttachmentStatus {
    override def included: Boolean = false
  }
}

case class ReviewedDependency(
  information: DependencyInformation,
  licenseReviewed: Option[Path],
  files: Seq[(AttachedFile, AttachmentStatus)],
  copyrights: Seq[(CopyrightMention, AttachmentStatus)]
)

case class ReviewedSummary(dependencies: Seq[ReviewedDependency]) {
  def warnings: Seq[String] =
    dependencies.flatMap { dep =>
      val warnings = collection.mutable.Buffer[String]()
      val name     = dep.information.moduleInfo.toString
      if (dep.licenseReviewed.isEmpty) {
        warnings.append(
          s"License ${dep.information.license.name} for $name is not reviewed."
        )
      }

      val missingFiles = dep.files.filter(_._2 == AttachmentStatus.NotReviewed)
      if (missingFiles.nonEmpty) {
        warnings.append(
          s"${missingFiles.size} files are not reviewed in $name."
        )
      }
      val missingCopyrights =
        dep.copyrights.filter(_._2 == AttachmentStatus.NotReviewed)
      if (missingCopyrights.nonEmpty) {
        warnings.append(
          s"${missingCopyrights.size} copyrights are not reviewed in $name."
        )
      }

      val includedInfos =
        (dep.files.map(_._2) ++ dep.copyrights.map(_._2)).filter(_.included)
      if (includedInfos.isEmpty) {
        warnings.append(
          s"No files or copyright information beside the license file are " +
          s"included for $name."
        )
      }

      warnings
    }
}
