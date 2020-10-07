package src.main.scala.licenses.report

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.Base64

import sbt.{File, IO}
import src.main.scala.licenses.review.Review
import src.main.scala.licenses.{
  AttachedFile,
  AttachmentStatus,
  DistributionDescription,
  ReviewedDependency,
  ReviewedSummary
}

object Report {
  def writeHTML(
    description: DistributionDescription,
    summary: ReviewedSummary,
    warnings: Seq[String],
    destination: File
  ): Unit = {
    val writer = HTMLWriter.toFile(destination)
    try {
      writer.writeHeader(s"Dependency summary for ${description.artifactName}")
      writeDescription(writer, description)
      writeWarningsSummary(writer, warnings)
      writeDependencySummary(writer, summary)

      writer.writeList(warnings.map { warning => () =>
        writer.writeText(warning)
      })

      writer.writeCollapsible("NOTICE header", summary.noticeHeader)
      if (summary.additionalFiles.nonEmpty) {
        writer.writeSubHeading("Additional files that will be added:")
        writer.writeList(summary.additionalFiles.map { file => () =>
          writer.writeText(file.path.toString)
        })
      }

    } finally {
      writer.close()
    }
  }

  private def writeDescription(
    writer: HTMLWriter,
    description: DistributionDescription
  ): Unit = {
    writer.writeHeader(
      s"Gathered Information on ${description.artifactName} Distribution"
    )
    writer.writeParagraph(
      s"Root components analyzed for the distribution: " +
      s"${description.componentsNames.mkString(", ")}."
    )
  }

  private def writeWarningsSummary(
    writer: HTMLWriter,
    warnings: Seq[String]
  ): Unit = {
    if (warnings.isEmpty) {
      writer.writeParagraph("There are no warnings.")
    } else {
      writer.writeParagraph(s"There are ${warnings.size} warnings!", Style.Bold)
    }
  }

  private def renderStatus(attachmentStatus: AttachmentStatus): String = {
    val style = attachmentStatus match {
      case AttachmentStatus.Keep            => Style.Black
      case AttachmentStatus.KeepWithContext => Style.Black
      case AttachmentStatus.Ignore          => Style.Gray
      case AttachmentStatus.Added           => Style.Green
      case AttachmentStatus.NotReviewed     => Style.Red
    }
    s"""<span style="$style">$attachmentStatus</span>"""
  }

  private def renderSimilarity(
    licensePath: Option[Path],
    file: AttachedFile,
    status: AttachmentStatus
  ): String = {
    val name = file.path.getFileName.toString.toLowerCase
    if (name.contains("license") || name.contains("licence")) {
      licensePath match {
        case Some(value) =>
          val defaultText = IO.read(value.toFile)
          if (defaultText.strip() == file.content.strip()) {
            val color = if (status.included) Style.Red else Style.Green
            s"""<span style="$color">100% identical to default license</span>"""
          } else
            s"""<span style="${Style.Red}">Differs from used license!</span>"""
        case None => ""
      }
    } else ""
  }

  private def writeDependencySummary(
    writer: HTMLWriter,
    summary: ReviewedSummary
  ): Unit = {
    val sorted = summary.dependencies.sortBy(dep =>
      (dep.information.moduleInfo.organization, dep.information.moduleInfo.name)
    )

    writer.writeSubHeading("Summary of all compile dependencies")
    writer.writeTable(
      headers = Seq(
        "Organization",
        "Name",
        "Version",
        "URL",
        "License",
        "Attached files",
        "Possible copyrights"
      ),
      rows = sorted.map {
        case dep @ ReviewedDependency(
              information,
              licenseReviewed,
              licensePath,
              files,
              copyrights
            ) =>
          (rowWriter: writer.RowWriter) =>
            val moduleInfo = information.moduleInfo
            rowWriter.addColumn(moduleInfo.organization)
            rowWriter.addColumn(moduleInfo.name)
            rowWriter.addColumn(moduleInfo.version)
            rowWriter.addColumn {
              information.url match {
                case Some(value) =>
                  writer.writeLink(value, value)
                case None => writer.writeText("No URL")
              }
            }
            val license = information.license
            rowWriter.addColumn {
              writer.writeLink(license.name, license.url)
              writer.writeText(s"(${license.category.name}) <br>")
              licensePath match {
                case Some(path) =>
                  writer.writeText(
                    path.getFileName.toString,
                    if (licenseReviewed) Style.Green else Style.Red
                  )
                case None if licenseReviewed =>
                  val licenseFile = summary.keptLicense(dep)
                  licenseFile match {
                    case Some(file) =>
                      writer.writeText(
                        s"Custom license ${file.path.getFileName}",
                        Style.Green
                      )
                    case None =>
                      writer.writeText(
                        "Custom license defined but not provided!",
                        Style.Red
                      )
                  }
                case None if !licenseReviewed =>
                  val name = Review.normalizeName(license.name)
                  writer.writeText(s"<pre>$name</pre>Not reviewed", Style.Red)
              }
            }
            rowWriter.addColumn {
              if (files.isEmpty) writer.writeText("No attached files.")
              else
                writer.writeList(files.map {
                  case (file, status) =>
                    () =>
                      val injection = writer.makeInjectionHandler(
                        "file-ui",
                        "package"  -> dep.information.packageName,
                        "filename" -> file.path.toString,
                        "status"   -> status.toString
                      )
                      val origin = file.origin
                        .map(origin => s" (Found at $origin)")
                        .getOrElse("")
                      writer.writeCollapsible(
                        s"${file.fileName} (${renderStatus(status)})$origin " +
                        s"${renderSimilarity(licensePath, file, status)}",
                        injection +
                        file.content
                      )
                })
            }
            rowWriter.addColumn {
              if (copyrights.isEmpty) {
                val bothEmpty = files.isEmpty && copyrights.isEmpty
                if (bothEmpty) {
                  writer.writeText(
                    "No notices or copyright information found, " +
                    "this may be a problem.",
                    Style.Red
                  )
                } else {
                  writer.writeText("No copyright information found.")
                }
              } else
                writer.writeList(copyrights.map {
                  case (mention, status) =>
                    () =>
                      val foundAt = mention.origins match {
                        case Seq()    => ""
                        case Seq(one) => s"Found at $one"
                        case Seq(first, rest @ _*) =>
                          s"Found at $first and ${rest.size} other files."
                      }

                      val contexts = if (mention.contexts.nonEmpty) {
                        mention.contexts
                          .map("<pre>\n" + _ + "\n</pre>")
                          .mkString("<hr>")
                      } else ""

                      val injection = writer.makeInjectionHandler(
                        "copyright-ui",
                        "package" -> dep.information.packageName,
                        "content" -> Base64.getEncoder.encodeToString(
                          mention.content.getBytes(StandardCharsets.UTF_8)
                        ),
                        "contexts" -> mention.contexts.length.toString,
                        "status"   -> status.toString
                      )

                      val content  = writer.escape(mention.content)
                      val moreInfo = injection + foundAt + contexts
                      writer.writeCollapsible(
                        s"$content (${renderStatus(status)})",
                        moreInfo
                      )
                })
            }
      }
    )
  }
}
