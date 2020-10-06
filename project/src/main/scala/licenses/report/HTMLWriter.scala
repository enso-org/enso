package src.main.scala.licenses.report

import java.io.{BufferedWriter, PrintWriter}
import java.nio.file.Files

import sbt.File

class HTMLWriter(bufferedWriter: BufferedWriter) {
  val writer = new PrintWriter(bufferedWriter)
  def writeHeader(title: String): Unit = {
    val heading =
      s"""<head>
         |<meta charset="utf-8">

         |  <script src="https://code.jquery.com/jquery-1.12.4.js"></script>
         |  <script src="https://code.jquery.com/ui/1.12.1/jquery-ui.js"></script>
         |<title>$title</title>
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

  class RowWriter(columns: Int) {
    var added = 0

    def addColumn(text: String): Unit = {
      addColumn(writeText(text))
    }

    def addColumn(writeAction: => Unit): Unit = {
      writer.println("<td>")
      writeAction
      writer.println("</td>")
      added += 1
    }

    def finish(): Unit = {
      if (added < columns)
        throw new IllegalStateException("Not enough columns added")
    }
  }

  def writeTable(headers: Seq[String], rows: Seq[RowWriter => Unit]): Unit = {
    writer.println("<table>")
    writer.println("<tr>")
    for (header <- headers) {
      writer.println(s"<th>$header</th>")
    }
    writer.println("</tr>")

    val columns = headers.length
    for (row <- rows) {
      writer.println("<tr>")
      val rw = new RowWriter(columns)
      row(rw)
      rw.finish()
      writer.println("</tr>")
    }

    writer.println("</table>")
  }

  def writeList(elements: Seq[() => Unit]): Unit = {
    writer.println("<ul>")
    for (elem <- elements) {
      writer.println("<li>")
      elem()
      writer.println("</li>")
    }
    writer.println("</ul>")
  }

  def writeLink(text: String, url: String): Unit =
    writer.println(s"""<a href="$url">$text</a>""")

  def writeHeading(heading: String): Unit =
    writer.println(s"<h1>$heading</h1>")

  def writeSubHeading(heading: String): Unit =
    writer.println(s"<h2>$heading</h2>")

  def writeParagraph(text: String, styles: Style*): Unit =
    writer.println(s"""<p style="${styles.mkString(";")}">$text</p>""")

  def writeText(text: String): Unit = writer.println(text)

  def writeCollapsible(
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

  def close(): Unit = {
    writer.println("</body>")
    writer.close()
  }
}

object HTMLWriter {
  def toFile(destination: File): HTMLWriter =
    new HTMLWriter(Files.newBufferedWriter(destination.toPath))
}

sealed trait Style
object Style {
  case object Bold extends Style {
    override def toString: String = "font-weight:bold"
  }
}
