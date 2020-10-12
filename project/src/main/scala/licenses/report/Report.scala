package src.main.scala.licenses.report

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util.Base64

import sbt.{File, IO}
import src.main.scala.licenses._

/**
  * Allows to write a report summarizing current status of the review.
  *
  * The report lists all dependencies and status of found attachments.
  *
  * With the legal-review-helper script, the generated report can be used to
  * interactively perform substantial parts of the review.
  */
object Report {

  /**
    * Writes the report in HTML format.
    *
    * @param description description of the distribution
    * @param summary reviewed summary of findings
    * @param warnings sequence of warnings
    * @param destination location of the generated HTML file
    */
  def writeHTML(
    description: DistributionDescription,
    summary: ReviewedSummary,
    warnings: Seq[String],
    destination: File
  ): Unit = {
    IO.createDirectory(destination.getParentFile)
    val writer = HTMLWriter.toFile(destination)
    try {
      writer.writeHeader(s"Dependency summary for ${description.artifactName}")
      writer.writeHeader(
        s"Gathered Information on ${description.artifactName} Distribution"
      )
      writer.writeParagraph(
        s"Root components analyzed for the distribution: " +
        s"${description.rootComponentsNames.mkString(", ")}."
      )

      if (warnings.isEmpty) {
        writer.writeParagraph("There are no warnings.", Style.Green)
      } else {
        writer.writeParagraph(
          s"There are ${warnings.size} warnings!",
          Style.Bold,
          Style.Red
        )
      }

      writeDependencySummary(writer, summary)

      writer.writeList(warnings.map { warning => () =>
        writer.writeText(writer.escape(warning))
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

  /**
    * Renders [[AttachmentStatus]] as HTML that will show the status name and a
    * color associated with it.
    */
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

  /**
    * Renders a message about similarity of the file to a selected license file.
    *
    * If the filename is license-like, the file is compared with the selected
    * license (if any). If the file differs from the selected license and is
    * ignored, a warning is displayed. If the file is kept but is identical (so
    * it is redundant) the message is also displayed as a minor warning.
    */
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

  /**
    * Writes a table containing summary of dependencies and their gathered
    * copyright information.
    */
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
                  val licenseFile = summary.includedLicense(dep)
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
                  writer.writeText(
                    s"Not reviewed, filename: <pre>$name</pre>",
                    Style.Red
                  )
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
                        writer.escape(file.content)
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
                          .map(c => "\n" + writer.escape(c) + "\n")
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
