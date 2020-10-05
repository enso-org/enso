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
    val writer = new PrintWriter(Files.newBufferedWriter(destination.toPath))
    try {
      writeHeading(writer, description.artifactName)
      writeDescription(writer, description)
      writeWarnings(writer, warnings)
      writeLicenseSummary(writer, summary)
      writeDependencySummary(writer, summary)
      writer.println("""</body>""")
    } finally {
      writer.close()
    }
  }

  private def writeHeading(writer: PrintWriter, name: String): Unit = {
    val heading =
      s"""<head>
         |<meta charset="utf-8">

         |  <script src="https://code.jquery.com/jquery-1.12.4.js"></script>
         |  <script src="https://code.jquery.com/ui/1.12.1/jquery-ui.js"></script>
         |<title>Dependency summary for $name</title>
         |<style>
         |table, th, td {
         | border: solid 1px;
         |}
         |h4 {
         |  font-weight: normal;
         |  display: inline;
         |}
         |</style>
         | <script>
         |  $$( function() {
         |    $$( ".accordion" ).accordion({
         |      active: false,
         |      collapsible: true
         |    });
         |  } );
         |  </script>
         |</head>
         |<body>""".stripMargin
    writer.println(heading)
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

  private def closeTable(writer: PrintWriter): Unit = {
    writer.println("</table>")
  }

  private def writeDescription(
    writer: PrintWriter,
    description: DistributionDescription
  ): Unit = {
    writer.println(
      s"<h1>Gathered Information on ${description.artifactName} " +
      s"Distribution</h1>"
    )
    writer.println(
      s"Root components analyzed for the distribution: " +
      s"${description.componentsNames.mkString(", ")}.<hr>"
    )
  }

  private def writeLicenseSummary(
    writer: PrintWriter,
    summary: DependencySummary
  ): Unit = {
    writer.println("<h2>Licenses present within dependencies</h2>")
    writer.println("<ul>")
    val licenses = summary.dependencies.map(_._1.license).distinct
    for (license <- licenses) {
      val status = "TODO license review status"
      writer.println(
        s"""<li><a href="${license.url}">${license.name}</a> - $status</li>"""
      )
    }
    writer.println("</ul>")
  }

  private def writeWarnings(
    writer: PrintWriter,
    warnings: Seq[String]
  ): Unit = {
    if (warnings.isEmpty) {
      writer.println("There are no warnings.")
    } else {
      writer.println(s"<b>There are ${warnings.size} warnings!</b>")
    }
    writer.println("<ul>")
    for (warning <- warnings) {
      writer.println(s"<li>$warning</li>")
    }
    writer.println("</ul>")
  }

  private def writeDependencySummary(
    writer: PrintWriter,
    summary: DependencySummary
  ): Unit = {
    val sorted = summary.dependencies.sortBy(dep =>
      (dep._1.moduleInfo.organization, dep._1.moduleInfo.name)
    )

    writer.println("<h2>Summary of all compile dependencies</h2>")
    openTable(writer)
    for ((dependencyInformation, attachments) <- sorted) {
      writer.println("<tr>")
      val moduleInfo = dependencyInformation.moduleInfo
      writer.println(s"<td>${moduleInfo.organization}</td>")
      writer.println(s"<td>${moduleInfo.name}</td>")
      writer.println(s"<td>${moduleInfo.version}</td>")

      dependencyInformation.url match {
        case Some(value) =>
          writer.println(s"""<td><a href="$value">$value</a></td>""")
        case None => writer.println("<td>No URL.</td>")
      }

      val license = dependencyInformation.license
      writer.println(
        s"""<td><a href="${license.url}">${license.name}</a> 
           |(${license.category.name}) </td>""".stripMargin
      )

      val notices    = attachments.collect { case n: Notice => n }
      val copyrights = attachments.collect { case c: CopyrightMention => c }

      val attachedLicense = notices.find(
        _.path.getFileName.toString.toLowerCase.contains("license")
      )

      writer.println("<td>")
      attachedLicense match {
        case Some(attachedLicenseFile) =>
          writeCollapsible(
            writer,
            attachedLicenseFile.fileName,
            attachedLicenseFile.content
          )
        case None =>
          writeCollapsible(
            writer,
            s"Not attached - Default ${license.name} text",
            "TODO content"
          )
      }
      writer.println("</td>")

      writer.println("<td>")
      if (notices.nonEmpty) {
        writer.println("<ul>")
        for (file <- notices) {
          writer.print("<li>")
          writeCollapsible(writer, file.fileName, file.content)
          writer.print("</li>")
        }
        writer.println("</ul>")
      } else {
        writer.println("No attached files.")
      }
      writer.println("</td>")

      writer.println("<td>")
      if (copyrights.nonEmpty) {
        writer.println("<ul>")
        for (mention <- copyrights) {
          writer.print("<li>")
          val foundAt =
            if (mention.origins.size == 1) s"Found at ${mention.origins.head}"
            else
              s"Found at ${mention.origins.head} and ${mention.origins.size - 1} other files."

          val context = mention.context
            .map("<br>Context: <pre>\n" + _ + "\n</pre>")
            .getOrElse("") // TODO
          val moreInfo = foundAt + context
          writeCollapsible(writer, mention.content, moreInfo)
          writer.print("</li>")
        }
        writer.println("</ul>")
      } else {
        writer.println("No copyright information found.")
      }
      writer.println("</td>")

      writer.println("</tr>")
    }

    closeTable(writer)
  }

  private def writeCollapsible(
    writer: PrintWriter,
    title: String,
    content: String
  ): Unit = {
    writer.println(s"""<div class="accordion">
                      |<h4>$title</h4>
                      |<div>
                      |<pre>
                      |$content
                      |</pre>
                      |</div></div>""".stripMargin)
  }
}
