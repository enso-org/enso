package src.main.scala.licenses.report

import sbt.File
import src.main.scala.licenses.{
  AttachedFile,
  AttachmentStatus,
  CopyrightMention,
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
        case ReviewedDependency(
              information,
              licenseReviewed,
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
              licenseReviewed match {
                case Some(path) =>
                  writer.writeText(path.getFileName.toString, Style.Green)
                case None =>
                  writer.writeText("Not reviewed", Style.Red)
              }
            }
            rowWriter.addColumn {
              if (files.isEmpty) writer.writeText("No attached files.")
              else
                writer.writeList(files.map {
                  case (file, status) =>
                    () =>
                      writer.writeCollapsible(
                        s"${file.fileName} (${renderStatus(status)})",
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
                      val moreInfo = foundAt + contexts
                      writer.writeCollapsible(
                        s"${mention.content} (${renderStatus(status)})",
                        moreInfo
                      )
                })
            }
      }
    )
  }
}
