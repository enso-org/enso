package org.enso.docs.generator

import java.io._
import scala.util.Using
import scala.io.Source
import scalatags.Text.{all => HTML}
import TreeOfCommonPrefixes._
import DocsGenerator._
import HTML._

/** The entry point for the documentation generator.
  *
  * The documentation generator is responsible for creating HTML documentation
  * for the Enso standard library.
  * It also generates JavaScript files containing react components for the
  * [[https://enso.org/docs/reference reference website]].
  */
object Main extends App {

  /// Files
  val path = "./distribution/std-lib/Standard/src"
  val allFiles = traverse(new File(path)).filter(f =>
    f.isFile && f.getName.endsWith(".enso")
  )
  val allFileNames = allFiles.map(
    _.getPath
      .replace(path + "/", "")
      .replace(".enso", "")
  )

  /// HTML's w/o react & tree (for IDE)
  val allPrograms = allFiles
    .map(f => Using(Source.fromFile(f, "UTF-8")) { s => s.mkString })
    .toList
  val allDocs = allPrograms
    .map(s => generate(s.getOrElse("")))
    .map(mapIfEmpty)
    .map(removeUnnecessaryDivs)
  val allDocFiles = allFiles.map(
    _.getPath
      .replace(".enso", ".html")
      .replace("Standard/src", "docs")
  )
  val zipped = allDocFiles.zip(allDocs)
  zipped.foreach(createDocHTMLFile)

  /// HTML's for syntax website.
  val libPath =
    "./lib/scala/docs-generator/src/main/scala/org/enso/docs/generator/"
  val treeNames    = groupByPrefix(allFileNames.toList).filter(_.elems.nonEmpty)
  val jsTemplate   = new File(libPath + "template.js")
  val templateCode = Using(Source.fromFile(jsTemplate, "UTF-8")) { _.mkString }
  val styleFile    = new File(libPath + "treeStyle.css")
  val styleCode    = Using(Source.fromFile(styleFile, "UTF-8")) { _.mkString }
  val treeStyle    = "<style jsx>{`" + styleCode.getOrElse("") + "`}</style>"
  val allDocJSFiles = allFiles.map { x =>
    val outDir = "docs-js"
    val name = x.getPath
      .replace(".enso", ".js")
      .replace("Standard/src", outDir)
      .replace("Main.js", "index.js")
    val ending = name.split(outDir + "/").tail.head
    name.replace(ending, ending.replace('/', '-'))
  }
  val dir = new File(allDocJSFiles.head.split("docs-js").head + "docs-js/")
  dir.mkdirs()
  val zippedJS = allDocJSFiles.zip(allDocs)
  zippedJS.foreach(createDocJSFile)

  /** Takes a tuple of file path and documented HTML code, saving the file
    * in the given directory.
    */
  private def createDocHTMLFile(x: (String, String)): Unit = {
    val path                   = x._1
    val file                   = new File(path)
    val fileWithExtensionRegex = "\\/[a-zA-Z_]*\\.[a-zA-Z]*"
    val dir                    = new File(path.replaceAll(fileWithExtensionRegex, ""))
    dir.mkdirs()
    file.createNewFile();
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(x._2)
    bw.close()
  }

  /** Takes a tuple of file path and documented HTML code, and generates JS doc
    * file with react components for Enso website.
    */
  private def createDocJSFile(x: (String, String)): Unit = {
    val path     = x._1
    val htmlCode = x._2
    val file     = new File(path)
    file.createNewFile();
    val bw = new BufferedWriter(new FileWriter(file))
    var treeCode =
      "<div>" + treeStyle + HTML
        .ul(treeNames.map(_.html()))
        .render
        .replace("class=", "className=") + "</div>"
    if (path.contains("/index.js")) {
      treeCode = treeCode.replace("a href=\"", "a href=\"reference/")
    }
    val partials = path
      .split("docs-js/")
      .tail
      .head
      .replace(".js", "")
      .split("-")
    for (i <- 1 to partials.length) {
      val id  = partials.take(i).mkString("-")
      val beg = "<input type=\"checkbox\" id=\"" + id + "\" "
      treeCode = treeCode.replace(beg, beg + "checked=\"True\"")
    }
    bw.write(
      templateCode
        .getOrElse("")
        .replace(
          "{/*PAGE*/}",
          htmlCode
            .replace("doc-copy-btn flex", "doc-copy-btn none")
            .replace("{", "&#123;")
            .replace("}", "&#125;")
        )
        .replace("{/*BREADCRUMBS*/}", treeCode)
    )
    bw.close()
  }
}
