package org.enso.docs.generator

import java.io._
import org.apache.commons.cli.{Option => CliOption, _}
import scala.util.{Try, Using}
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
object Main {

  var libPath =
    "./lib/scala/docs-generator/src/main/scala/org/enso/docs/generator/"
  var path           = "./distribution/std-lib/Standard/src"
  var jsTempFileName = "template.js"
  var cssFileName    = "treeStyle.css"
  var outDir         = "docs-js"

  private val HELP_OPTION          = "help"
  private val INPUT_PATH_OPTION    = "input-path"
  private val OUTPUT_DIR_OPTION    = "output-dir"
  private val DOCS_LIB_PATH_OPTION = "docs-lib-path"
  private val JS_TEMPLATE_OPTION   = "js-template"
  private val CSS_TEMPLATE_OPTION  = "css-template"

  /** Builds the [[Options]] object representing the CLI syntax.
    *
    * @return an [[Options]] object representing the CLI syntax
    */
  private def buildOptions = {
    val help = CliOption
      .builder("h")
      .longOpt(HELP_OPTION)
      .desc("Displays this message.")
      .build
    val input = CliOption.builder
      .longOpt(INPUT_PATH_OPTION)
      .numberOfArgs(1)
      .argName("path")
      .desc("Specifies working path.")
      .build
    val output = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("directory")
      .longOpt(OUTPUT_DIR_OPTION)
      .desc("Specifies name of output directory.")
      .build
    val docsPath = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("path")
      .longOpt(DOCS_LIB_PATH_OPTION)
      .desc("Specifies path of Docs Generator library.")
      .build
    val jsTemp = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("file")
      .longOpt(JS_TEMPLATE_OPTION)
      .desc("Specifies name of file containing JS template.")
      .build
    val cssTemp = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("file")
      .longOpt(CSS_TEMPLATE_OPTION)
      .desc("Specifies name of file containing CSS template.")
      .build

    val options = new Options
    options
      .addOption(help)
      .addOption(input)
      .addOption(output)
      .addOption(docsPath)
      .addOption(jsTemp)
      .addOption(cssTemp)

    options
  }

  /** Prints the help message to the standard output.
    *
    * @param options object representing the CLI syntax
    */
  private def printHelp(options: Options): Unit =
    new HelpFormatter().printHelp("Docs Generator", options)

  /** Terminates the process with a failure exit code. */
  private def exitFail(): Nothing = sys.exit(1)

  /** Terminates the process with a success exit code. */
  private def exitSuccess(): Nothing = sys.exit(0)

  /** Starting point. */
  def main(args: Array[String]): Unit = {
    val options = buildOptions
    val parser  = new DefaultParser
    val line = Try(parser.parse(options, args)).getOrElse {
      printHelp(options)
      exitFail()
    }
    if (line.hasOption(HELP_OPTION)) {
      printHelp(options)
      exitSuccess()
    }
    if (line.hasOption(INPUT_PATH_OPTION)) {
      path = line.getOptionValue(INPUT_PATH_OPTION)
    }
    if (line.hasOption(OUTPUT_DIR_OPTION)) {
      outDir = line.getOptionValue(OUTPUT_DIR_OPTION)
    }
    if (line.hasOption(DOCS_LIB_PATH_OPTION)) {
      libPath = line.getOptionValue(DOCS_LIB_PATH_OPTION)
    }
    if (line.hasOption(JS_TEMPLATE_OPTION)) {
      jsTempFileName = line.getOptionValue(JS_TEMPLATE_OPTION)
    }
    if (line.hasOption(CSS_TEMPLATE_OPTION)) {
      cssFileName = line.getOptionValue(CSS_TEMPLATE_OPTION)
    }

    generateAllDocs()
  }

  /** Traverses through directory generating docs from every .enso file found.
    */
  def generateAllDocs(): Unit = {
    val allFiles = traverse(new File(path))
      .filter(f => f.isFile && f.getName.endsWith(".enso"))
    val allFileNames = allFiles.map(
      _.getPath
        .replace(path + "/", "")
        .replace(".enso", "")
    )
    val allPrograms = allFiles
      .map(f => Using(Source.fromFile(f, "UTF-8")) { _.mkString })
      .toList
    val allDocs = allPrograms
      .map(s => run(s.getOrElse("")))
      .map(mapIfEmpty)
      .map(removeUnnecessaryDivs)
    val treeNames =
      groupByPrefix(allFileNames.toList, '/').filter(_.elems.nonEmpty)
    val jsTemplate = new File(libPath + jsTempFileName)
    val templateCode = Using(Source.fromFile(jsTemplate, "UTF-8")) {
      _.mkString
    }
    val styleFile = new File(libPath + cssFileName)
    val styleCode = Using(Source.fromFile(styleFile, "UTF-8")) { _.mkString }
    val treeStyle = "<style jsx>{`" + styleCode.getOrElse("") + "`}</style>"
    val allDocJSFiles = allFiles.map { x =>
      val name = x.getPath
        .replace(".enso", ".js")
        .replace("Standard/src", outDir)
        .replace("Main.js", "index.js")
      val ending = name.split(outDir + "/").tail.head
      name.replace(ending, ending.replace('/', '-'))
    }
    val dir = new File(allDocJSFiles.head.split(outDir).head + outDir + "/")
    dir.mkdirs()
    val zippedJS = allDocJSFiles.zip(allDocs)
    zippedJS.foreach(
      createDocJSFile(_, outDir, treeStyle, templateCode, treeNames)
    )
  }

  /** Takes a tuple of file path and documented HTML code, and generates JS doc
    * file with react components for Enso website.
    */
  private def createDocJSFile(
    x: (String, String),
    outDir: String,
    treeStyle: String,
    templateCode: Try[String],
    treeNames: List[Node]
  ): Unit = {
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
      .split(outDir + "/")
      .tail
      .head
      .replace(".js", "")
      .split("-")
    for (i <- 1 to partials.length) {
      val id  = partials.take(i).mkString("-")
      val beg = "<input type=\"checkbox\" id=\"" + id + "\" "
      treeCode = treeCode.replace(beg, beg + "checked=\"True\"")
    }
    val docCopyBtnClass = "doc-copy-btn"
    bw.write(
      templateCode
        .getOrElse("")
        .replace(
          "{/*PAGE*/}",
          htmlCode
            .replace(docCopyBtnClass + " flex", docCopyBtnClass + " none")
            .replace("{", "&#123;")
            .replace("}", "&#125;")
        )
        .replace("{/*BREADCRUMBS*/}", treeCode)
    )
    bw.close()
  }
}
