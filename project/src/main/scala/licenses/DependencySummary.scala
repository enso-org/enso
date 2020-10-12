package src.main.scala.licenses

import java.nio.file.Path

import sbt.IO
import src.main.scala.licenses.report.WithWarnings

/**
  * Contains a sequence of dependencies and any attachments found.
  */
case class DependencySummary(
  dependencies: Seq[(DependencyInformation, Seq[Attachment])]
)

/**
  * Review status of the [[Attachment]].
  */
sealed trait AttachmentStatus {

  /**
    * Determines if the attachment with this status should be included in the
    * final package.
    */
  def included: Boolean
}
object AttachmentStatus {

  /**
    * Indicates that the attachment should be kept.
    */
  case object Keep extends AttachmentStatus {

    /**
      * @inheritdoc
      */
    override def included: Boolean = true
  }

  /**
    * Indicates that the copyright mention should be kept, but its whole context
    * should be used instead of its content.
    *
    * Only valid for [[CopyrightMention]].
    */
  case object KeepWithContext extends AttachmentStatus {

    /**
      * @inheritdoc
      */
    override def included: Boolean = true
  }

  /**
    * Indicates that the attachment should be ignored.
    */
  case object Ignore extends AttachmentStatus {

    /**
      * @inheritdoc
      */
    override def included: Boolean = false
  }

  /**
    * Indicates that the attachment has been added manually.
    */
  case object Added extends AttachmentStatus {

    /**
      * @inheritdoc
      */
    override def included: Boolean = true
  }

  /**
    * Indicates that the attachment was not yet reviewed.
    */
  case object NotReviewed extends AttachmentStatus {

    /**
      * @inheritdoc
      */
    override def included: Boolean = false
  }
}

/**
  * Gathers information related to a dependency after the review.
  *
  * @param information original [[DependencyInformation]]
  * @param licenseReviewed indicates if the license associated with the
  *                        dependency is marked as reviewed
  * @param licensePath may contain a path to the default license file that will
  *                    be used; if empty, `files` should contain an alternative
  *                    license
  * @param files list of files attached to the dependency, with their review
  *              statuses
  * @param copyrights list of copyright mentions attached to the dependency,
  *                   with their review statuses
  */
case class ReviewedDependency(
  information: DependencyInformation,
  licenseReviewed: Boolean,
  licensePath: Option[Path],
  files: Seq[(AttachedFile, AttachmentStatus)],
  copyrights: Seq[(CopyrightMention, AttachmentStatus)]
)

/**
  * Summarizes the dependency review.
  *
  *  The reviewed version of [[DependencySummary]].
  *
  * @param dependencies sequence of reviewed dependencies
  * @param noticeHeader header to include in the generated NOTICE
  * @param additionalFiles additional files that should be added to the root of
  *                        the notice package
  */
case class ReviewedSummary(
  dependencies: Seq[ReviewedDependency],
  noticeHeader: String,
  additionalFiles: Seq[AttachedFile]
) {

  /**
    * Returns a license-like file that is among attached files that are included
    * (if such file exists).
    */
  def includedLicense(dependency: ReviewedDependency): Option[AttachedFile] =
    dependency.files
      .find { f =>
        val isIncluded = f._2.included
        val name       = f._1.path.getFileName.toString.toLowerCase
        val isLicense  = name.contains("license") || name.contains("licence")
        isIncluded && isLicense
      }
      .map(_._1)
}

object ReviewedSummary {

  /**
    * Returns a list of warnings that indicate missing reviews or other issues.
    */
  def warnAboutMissingReviews(summary: ReviewedSummary): WithWarnings[Unit] = {
    val warnings = summary.dependencies.flatMap { dep =>
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
          s"No files or copyright information are included for $name."
        )
      }

      (summary.includedLicense(dep), dep.licensePath) match {
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
    WithWarnings.justWarnings(warnings)
  }
}
