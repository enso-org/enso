package org.enso.docs.generator

import java.io._
import org.apache.commons.cli.{Option => CliOption, _}
import scala.util.{Try, Using}
import scala.io.Source
import scalatags.Text.{all => HTML}
import TreeOfCommonPrefixes._
import DocParserWrapper._
import Constants._
import HTML._

/** The entry point for the documentation generator.
  *
  * The documentation generator is responsible for creating HTML documentation
  * for the Enso standard library.
  * It also generates JavaScript files containing react components for the
  * [[https://enso.org/docs/reference reference website]].
  */
object Main {

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
    val path =
      Option(line.getOptionValue(INPUT_PATH_OPTION)).getOrElse(SOURCE_PATH)
    val outDir =
      Option(line.getOptionValue(OUTPUT_DIR_OPTION)).getOrElse(OUTPUT_DIRECTORY)
    val templateFilesPath = Option(line.getOptionValue(DOCS_LIB_PATH_OPTION))
      .getOrElse(TEMPLATE_FILES_PATH)
    val jsTempFileName = Option(line.getOptionValue(JS_TEMPLATE_OPTION))
      .getOrElse(JS_TEMPLATE_NAME)
    val cssFileName = Option(line.getOptionValue(CSS_TEMPLATE_OPTION))
      .getOrElse(CSS_TREE_FILE_NAME)

    generateAllDocs(
      path,
      templateFilesPath,
      jsTempFileName,
      cssFileName,
      outDir
    )
  }

  /** Traverses through directory generating docs from every .enso file found.
    */
  def generateAllDocs(
    path: String,
    templateFilesPath: String,
    jsTempFileName: String,
    cssFileName: String,
    outDir: String
  ): Unit = {
    val allFiles = traverse(new File(path))
      .filter(f => f.isFile && f.getName.endsWith(".enso"))
    val allFileNames = allFiles.map(
      _.getPath
        .replace(path + "/", "")
        .replace(".enso", "")
        .replace("src/", "")
        .replace("/0.1.0/", "/")
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
    val jsTemplate = new File(templateFilesPath + jsTempFileName)
    val templateCode = Using(Source.fromFile(jsTemplate, "UTF-8")) {
      _.mkString
    }
    val styleFile = new File(templateFilesPath + cssFileName)
    val styleCode = Using(Source.fromFile(styleFile, "UTF-8")) { _.mkString }
    val treeStyle = "<style jsx>{`" + styleCode.getOrElse("") + "`}</style>"
    val allDocJSFiles = allFiles.map { x =>
      val name = x.getPath
        .replace(".enso", ".js")
        .replace("lib/Standard/", outDir + "/")
        .replace("Main.js", "index.js")
        // TODO [RW] update this once library versions are changing
        .replace("/0.1.0/", "/")
        .replace("src/", "")
      val ending = name.split(outDir + "/").tail.head
      name.replace(ending, ending.replace('/', '-'))
    }
    val dir = new File(allDocJSFiles.head.split(outDir).head + outDir + "/")
    dir.mkdirs()
    val zippedJS = allDocJSFiles.zip(allDocs)
    zippedJS.foreach(d =>
      createDocJSFile(d._1, d._2, outDir, treeStyle, templateCode, treeNames)
    )
  }

  /** Takes a tuple of file path and documented HTML code, and generates JS doc
    * file with react components for Enso website.
    */
  private def createDocJSFile(
    path: String,
    htmlCode: String,
    outDir: String,
    treeStyle: String,
    templateCode: Try[String],
    treeNames: List[Node]
  ): Unit = {
    val file = new File(path)
    file.createNewFile()
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
