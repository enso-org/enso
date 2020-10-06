package src.main.scala.licenses.report

import java.io.PrintWriter
import java.nio.file.Files

import sbt.File
import src.main.scala.licenses.{
  CopyrightMention,
  DependencySummary,
  DistributionDescription,
  Notice
}

object Report {
  def writeHTML(
    description: DistributionDescription,
    summary: DependencySummary,
    warnings: Seq[String],
    destination: File
  ): Unit = {
    val writer = HTMLWriter.toFile(destination)
    try {
      writer.writeHeader(s"Dependency summary for ${description.artifactName}")
      writeDescription(writer, description)
      writeWarnings(writer, warnings)
      writeLicenseSummary(writer, summary)
      writeDependencySummary(writer, summary)
    } finally {
      writer.close()
    }
  }

  private def openTable(writer: PrintWriter): Unit = {
    val headers =
      s"""<table>
         |<tr>
         |<th>Organization</th>
         |<th>Name</th>
         |<th>Version</th>
         |<th>URL</th>
         |<th>License</th>
         |<th>License file</th>
         |<th>Attached files</th>
         |<th>Possible copyrights</th>
         |</tr>
         |""".stripMargin
    writer.println(headers)
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

  private def writeLicenseSummary(
    writer: HTMLWriter,
    summary: DependencySummary
  ): Unit = {
    writer.writeSubHeading("Licenses present within dependencies")
    val licenses = summary.dependencies.map(_._1.license).distinct
    writer.writeList(licenses.map { license => () =>
      val status = "TODO license review status"
      writer.writeLink(s"${license.name}</a> - $status", license.url)
    })
  }

  private def writeWarnings(
    writer: HTMLWriter,
    warnings: Seq[String]
  ): Unit = {
    if (warnings.isEmpty) {
      writer.writeParagraph("There are no warnings.")
    } else {
      writer.writeParagraph(s"There are ${warnings.size} warnings!", Style.Bold)
    }
    writer.writeList(warnings.map { warning => () =>
      writer.writeText(warning)
    })
  }

  private def writeDependencySummary(
    writer: HTMLWriter,
    summary: DependencySummary
  ): Unit = {
    val sorted = summary.dependencies.sortBy(dep =>
      (dep._1.moduleInfo.organization, dep._1.moduleInfo.name)
    )

    writer.writeSubHeading("Summary of all compile dependencies")
    writer.writeTable(
      headers = Seq(
        "Organization",
        "Name",
        "Version",
        "URL",
        "License",
        "License file",
        "Attached files",
        "Possible copyrights"
      ),
      rows = sorted.map {
        case (dependencyInformation, attachments) =>
          (rowWriter: writer.RowWriter) =>
            val moduleInfo = dependencyInformation.moduleInfo
            rowWriter.addColumn(moduleInfo.organization)
            rowWriter.addColumn(moduleInfo.name)
            rowWriter.addColumn(moduleInfo.version)
            rowWriter.addColumn {
              dependencyInformation.url match {
                case Some(value) =>
                  writer.writeLink(value, value)
                case None => writer.writeText("No URL")
              }
            }
            val license = dependencyInformation.license
            rowWriter.addColumn {
              writer.writeLink(license.name, license.url)
              writer.writeText(s"(${license.category.name})")
            }

            val notices = attachments.collect { case n: Notice => n }
            val copyrights = attachments.collect {
              case c: CopyrightMention => c
            }

//            val attachedLicense = notices.find(
//              _.path.getFileName.toString.toLowerCase.contains("license")
//            )
//            attachedLicense match {
//              case Some(attachedLicenseFile) =>
//                writeCollapsible(
//                  writer,
//                  attachedLicenseFile.fileName,
//                  attachedLicenseFile.content
//                )
//              case None =>
//                writeCollapsible(
//                  writer,
//                  s"Not attached - Default ${license.name} text",
//                  "TODO content"
//                )
//            }
            rowWriter.addColumn("Attached license TODO")
            rowWriter.addColumn {
              if (notices.isEmpty) writer.writeText("No attached files.")
              else
                writer.writeList(notices.map { file => () =>
                  writer.writeCollapsible(file.fileName, file.content)
                })
            }
            rowWriter.addColumn {
              if (copyrights.isEmpty)
                writer.writeText("No copyright information found.")
              else
                writer.writeList(copyrights.map {
                  mention => () =>
                    val foundAt =
                      if (mention.origins.size == 1)
                        s"Found at ${mention.origins.head}"
                      else
                        s"Found at ${mention.origins.head} and " +
                        s"${mention.origins.size - 1} other files."
                    val contexts = if (mention.contexts.nonEmpty) {
                      mention.contexts
                        .map("<pre>\n" + _ + "\n</pre>")
                        .mkString("<hr>")
                    } else ""
                    val moreInfo = foundAt + contexts
                    writer.writeCollapsible(mention.content, moreInfo)
                })
            }
      }
    )
  }
}
