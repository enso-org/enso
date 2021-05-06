package org.enso

import java.io._

import scala.util.Using
import scala.io.Source
import scalatags.Text.{all => HTML}
import HTML._

object DocsGeneratorMain extends App {
  import docsgenerator.DocsGenerator._
  import docsgenerator.TreeOfCommonPrefixes._

  /// Files
  val path = "./distribution/std-lib/Standard/src"
  val allFiles = traverse(new File(path))
    .filter(f => f.isFile && f.getName.endsWith(".enso"))
  val allFileNames =
    allFiles.map(_.getPath.replace(path + "/", "").replace(".enso", ""))
  val allPrograms =
    allFiles
      .map(f => Using(Source.fromFile(f, "UTF-8")) { s => s.mkString })
      .toList

  /// HTML's w/o react & tree (for IDE)
  val allDocs =
    allPrograms
      .map(s => generate(s.getOrElse("")))
      .map(mapIfEmpty)
      .map(removeUnnecessaryDivs)
  val allDocFiles = allFiles.map(
    _.getPath.replace(".enso", ".html").replace("Standard/src", "docs")
  )
  val zipped = allDocFiles.zip(allDocs)
  zipped.foreach(x => {
    val file = new File(x._1)
    val dir  = new File(x._1.replaceAll("\\/[a-zA-Z_]*\\.[a-zA-Z]*", ""))
    dir.mkdirs()
    file.createNewFile();
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(x._2)
    bw.close()
  })

  /// HTML's for syntax website.
  val treeNames = groupByPrefix(allFileNames.toList).filter(_.elems.nonEmpty)
  val jsTemplate = new File(
    "./lib/scala/docs-generator/src/main/scala/org/enso/docsgenerator/template.js"
  )
  val templateCode = Using(Source.fromFile(jsTemplate, "UTF-8")) { s =>
    s.mkString
  }
  val treeStyleFile = new File(
    "./lib/scala/docs-generator/src/main/scala/org/enso/docsgenerator/treeStyle.css"
  )
  val treeStyleCode = Using(Source.fromFile(treeStyleFile, "UTF-8")) { s =>
    s.mkString
  }
  val treeStyle = "<style jsx>{`" + treeStyleCode.getOrElse("") + "`}</style>"
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
  zippedJS.foreach(x => {
    val file = new File(x._1)
    file.createNewFile();
    val bw = new BufferedWriter(new FileWriter(file))
    var treeCode =
      "<div>" + treeStyle + HTML
        .ul(treeNames.map(_.html()))
        .render
        .replace("class=", "className=") + "</div>"
    if (x._1.contains("/index.js")) {
      treeCode = treeCode.replace("a href=\"", "a href=\"reference/")
    }
    val partials =
      x._1.split("docs-js/").tail.head.replace(".js", "").split("-")
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
          x._2
            .replace("doc-copy-btn flex", "doc-copy-btn none")
            .replace("{", "&#123;")
            .replace("}", "&#125;")
        )
        .replace("{/*BREADCRUMBS*/}", treeCode)
    )
    bw.close()
  })
}
