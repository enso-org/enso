package src.main.scala.licenses.report

import java.io.{BufferedWriter, PrintWriter}
import java.nio.file.Files

import sbt.File

/**
  * Allows to create very simple HTML reports.
  *
  * @param bufferedWriter writer to which the generated code will be written
  */
class HTMLWriter(bufferedWriter: BufferedWriter) {
  val writer = new PrintWriter(bufferedWriter)

  /**
    * Writes the header containing basic styles and scripts for the reports.
    *
    * @param title the page title
    */
  def writeHeader(title: String): Unit = {
    val heading =
      s"""<html>
         |<head>
         |<meta charset="utf-8">
         |<script src="https://code.jquery.com/jquery-1.12.4.js"></script>
         |<script src="https://code.jquery.com/ui/1.12.1/jquery-ui.js"></script>
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
         |$$( function() {
         |  $$( ".accordion" ).accordion({
         |    active: false,
         |    collapsible: true
         |  });
         |});
         |</script>
         |</head>
         |<body>""".stripMargin
    writer.println(heading)
  }

  /**
    * A helper class that allows to manage columns in a table row.
    *
    * @param columns expected columns count
    */
  class RowWriter(columns: Int) {
    var added = 0

    /**
      * A helper function that adds a column just containing plain text.
      */
    def addColumn(text: String): Unit = {
      addColumn(writeText(text))
    }

    /**
      * Adds a cell in the next column that will contain anything that is
      * written when executing the `writeAction`.
      */
    def addColumn(writeAction: => Unit): Unit = {
      writer.println("<td>")
      writeAction
      writer.println("</td>")
      added += 1
    }

    /**
      * Finishes the current row and ensures that all expected columns have been
      * added.
      */
    def finish(): Unit = {
      if (added < columns)
        throw new IllegalStateException("Not enough columns added")
    }
  }

  /**
    * Writes a HTML table based on the provided descriptiom.
    *
    * @param headers sequence of header names for each column
    * @param rows sequence of functions that will be called to create rows of
    *             the table, each function is provided with a [[RowWriter]] that
    *             should write as many columns as there are headers
    */
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

  /**
    * Writes an unordered list.
    *
    * @param elements sequence of functions that will be called to write each of
    *                 the list's elements; everything written inside of each
    *                 function will be part of its list element
    */
  def writeList(elements: Seq[() => Unit]): Unit = {
    writer.println("<ul>")
    for (elem <- elements) {
      writer.println("<li>")
      elem()
      writer.println("</li>")
    }
    writer.println("</ul>")
  }

  /**
    * Writes a link.
    *
    * @param text link text
    * @param url link target
    */
  def writeLink(text: String, url: String): Unit =
    writer.println(s"""<a href="$url">$text</a>""")

  /**
    * Writes a H1 heading.
    */
  def writeHeading(heading: String): Unit =
    writer.println(s"<h1>$heading</h1>")

  /**
    * Writes a H2 heading.
    */
  def writeSubHeading(heading: String): Unit =
    writer.println(s"<h2>$heading</h2>")

  /**
    * Writes a paragraph of styled text.
    */
  def writeParagraph(text: String, styles: Style*): Unit =
    writer.println(s"""<p style="${styles.mkString(";")}">$text</p>""")

  /**
    * Writes plain (but potentially styled) text.
    */
  def writeText(text: String, styles: Style*): Unit =
    if (styles.nonEmpty)
      writer.println(s"""<span style="${styles.mkString(";")}">$text</span>""")
    else writer.println(text)

  /**
    * Writes a collapsible entry that by default just displays the `title`, but
    * expands when clicked to show the `content`.
    */
  def writeCollapsible(
    title: String,
    content: String
  ): Unit = {
    writer.println(s"""<div class="accordion">
                      |<h4>$title</h4>
                      |<div style="display:none">
                      |<pre>
                      |$content
                      |</pre>
                      |</div></div>""".stripMargin)
  }

  /**
    * Writes an empty element that can be overridden by an injected script.
    *
    * Parameters names and values need to be properly escaped to fit for HTML.
    */
  def makeInjectionHandler(
    className: String,
    params: (String, String)*
  ): String = {
    val mappedParams = params
      .map {
        case (name, value) => s"""data-$name="$value" """
      }
      .mkString(" ")
    s"""<span class="$className" $mappedParams></span>
       |""".stripMargin
  }

  /**
    * Escapes a string that may contain HTML markup to not be rendered but
    * displayed as normal text.
    */
  def escape(string: String): String =
    string.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

  /**
    * Finishes writing the document and closes the output.
    */
  def close(): Unit = {
    writer.println("</body></html>")
    writer.close()
  }
}

object HTMLWriter {

  /**
    * Creates an [[HTMLWriter]] that writes its output to a [[File]] (creating
    * it or overwriting if it exists).
    */
  def toFile(destination: File): HTMLWriter =
    new HTMLWriter(Files.newBufferedWriter(destination.toPath))
}

/**
  * Styles that can be used to make text stand out.
  */
sealed trait Style
object Style {

  /**
    * Makes the text bold.
    */
  case object Bold extends Style {
    override def toString: String = "font-weight:bold"
  }

  /**
    * Makes the text red.
    */
  case object Red extends Style {
    override def toString: String = "color:red"
  }

  /**
    * Makes the text gray.
    */
  case object Gray extends Style {
    override def toString: String = "color:gray"
  }

  /**
    * Makes the text green.
    */
  case object Green extends Style {
    override def toString: String = "color:green"
  }

  /**
    * Makes the text black.
    */
  case object Black extends Style {
    override def toString: String = "color:black"
  }
}
