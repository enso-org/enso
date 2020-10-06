package src.main.scala.licenses

import java.nio.file.Path

import sbt.IO

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
  licenseReviewed: Boolean,
  licensePath: Option[Path],
  files: Seq[(AttachedFile, AttachmentStatus)],
  copyrights: Seq[(CopyrightMention, AttachmentStatus)]
)

case class ReviewedSummary(
  dependencies: Seq[ReviewedDependency],
  noticeHeader: String,
  additionalFiles: Seq[AttachedFile]
) {

  def keptLicense(dependency: ReviewedDependency): Option[AttachedFile] =
    dependency.files
      .find { f =>
        val isKept    = f._2 == AttachmentStatus.Keep
        val name      = f._1.path.getFileName.toString.toLowerCase
        val isLicense = name.contains("license") || name.contains("licence")
        isKept && isLicense
      }
      .map(_._1)

  def warnings: Seq[String] =
    dependencies.flatMap { dep =>
      val warnings = collection.mutable.Buffer[String]()
      val name     = dep.information.moduleInfo.toString
      if (!dep.licenseReviewed) {
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

      (keptLicense(dep), dep.licensePath) match {
        case (Some(kept), Some(reviewedLicense)) =>
          val licenseContent = IO.read(reviewedLicense.toFile)
          if (licenseContent.strip != kept.content) {
            warnings.append(
              s"A license file was discovered in $name that is different " +
              s"from the default license file that is associated with its " +
              s"license ${dep.information.license.name}."
            )
          }
        case (None, None) =>
          warnings.append(
            s"The license for $name is set to <custom> but no license-like " +
            s"file is found."
          )
        case _ =>
      }

      warnings
    }
}
