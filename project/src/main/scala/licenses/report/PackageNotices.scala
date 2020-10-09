package src.main.scala.licenses.report

import java.nio.file.{Files, Path}

import sbt._
import src.main.scala.licenses.{
  AttachmentStatus,
  CopyrightMention,
  DistributionDescription,
  ReviewedSummary
}

import scala.annotation.tailrec

/**
  * Defines writing the notices package for distribution.
  */
object PackageNotices {

  /**
    * Creates the notices package based on the `summary` in `destination`.
    *
    * The `destination` directory is cleaned before creating the new notices.
    *
    * It copies licences, files and gathers copyright notices that were set to
    * be included in the review and generates a summary `NOTICE` file.
    */
  def create(
    description: DistributionDescription,
    summary: ReviewedSummary,
    destination: File
  ): Unit = {
    IO.delete(destination)
    IO.createDirectory(destination)
    if (IO.listFiles(destination).nonEmpty) {
      throw new RuntimeException(
        s"Could not clean ${destination}, cannnot continue creating the " +
        s"package."
      )
    }

    val artifactName = description.artifactName
    val mainNotice   = new StringBuilder
    mainNotice.append(summary.noticeHeader)

    val licensesRoot      = destination / "licenses"
    val processedLicenses = collection.mutable.Set[Path]()

    for (dependency <- summary.dependencies) {
      val name        = dependency.information.moduleInfo.name
      val licenseName = dependency.information.license.name
      mainNotice.append(
        s"\n\n'$name', licensed under the $licenseName, " +
        s"is distributed with the $artifactName.\n"
      )
      dependency.licensePath match {
        case Some(path) =>
          val name = path.getFileName.toString
          if (!processedLicenses.contains(path)) {
            val destination = licensesRoot / name
            IO.copyFile(path.toFile, destination)
            if (!destination.exists()) {
              throw new RuntimeException(s"Failed to copy the license $path")
            }
            processedLicenses.add(path)
          }
          mainNotice.append(
            s"The license file can be found at `licenses/$name`.\n"
          )
        case None =>
          mainNotice.append(
            s"The license file can be found at along the copyright notices.\n"
          )
      }

      val packageName = dependency.information.packageName
      val packageRoot = destination / packageName

      val files      = dependency.files.filter(_._2.included).map(_._1)
      val copyrights = dependency.copyrights.filter(_._2.included)

      if (files.size + copyrights.size > 0) {
        mainNotice.append(
          s"Copyright notices related to this dependency can be found in the " +
          s"directory `$packageName`.\n"
        )
        IO.createDirectory(packageRoot)
      }

      @tailrec
      def findFreeName(name: String, counter: Int = 0): File = {
        val actualName = if (counter > 0) s"$name.$counter" else name
        val file       = packageRoot / actualName
        if (Files.exists(file.toPath)) findFreeName(name, counter + 1)
        else file
      }

      for (attachedFile <- files) {
        val file = findFreeName(attachedFile.path.getFileName.toString)
        IO.write(file, attachedFile.content)
      }

      def renderCopyright(
        copyright: CopyrightMention,
        status: AttachmentStatus
      ): String =
        status match {
          case AttachmentStatus.Keep => copyright.content
          case AttachmentStatus.KeepWithContext =>
            if (copyright.contexts.size != 1) {
              throw new IllegalStateException(
                "`KeepWithContext` can only be used for copyrights that " +
                "have exactly one context."
              )
            }
            copyright.contexts.head
          case AttachmentStatus.Added =>
            copyright.contexts.head
          case _ =>
            throw new IllegalStateException(
              "Only `included` copyrights should be present at this point."
            )
        }

      if (copyrights.nonEmpty) {
        val compiledCopyrights = copyrights
          .map {
            case (m, s) => renderCopyright(m, s)
          }
          .mkString("\n\n")
        val freeName = findFreeName("NOTICES")
        IO.write(freeName, compiledCopyrights)
      }
    }

    IO.write(destination / "NOTICE", mainNotice.toString())
  }
}
