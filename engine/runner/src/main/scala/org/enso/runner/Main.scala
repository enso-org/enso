package org.enso.runner

import java.io.File
import java.util.UUID

import buildinfo.Info
import cats.implicits._
import org.apache.commons.cli.{Option => CliOption, _}
import org.enso.languageserver
import org.enso.languageserver.LanguageServerConfig
import org.enso.pkg.Package
import org.enso.polyglot.{LanguageInfo, Module, PolyglotContext}
import org.graalvm.polyglot.Value

import scala.annotation.unused
import scala.util.Try

/** The main CLI entry point class. */
object Main {

  private val RUN_OPTION             = "run"
  private val HELP_OPTION            = "help"
  private val NEW_OPTION             = "new"
  private val REPL_OPTION            = "repl"
  private val JUPYTER_OPTION         = "jupyter-kernel"
  private val LANGUAGE_SERVER_OPTION = "server"
  private val INTERFACE_OPTION       = "interface"
  private val PORT_OPTION            = "port"
  private val ROOT_ID_OPTION         = "root-id"
  private val ROOT_PATH_OPTION       = "path"
  private val VERSION_OPTION         = "version"

  /**
    * Builds the [[Options]] object representing the CLI syntax.
    *
    * @return an [[Options]] object representing the CLI syntax
    */
  private def buildOptions = {
    val help = CliOption
      .builder("h")
      .longOpt(HELP_OPTION)
      .desc("Displays this message.")
      .build
    val repl = CliOption.builder
      .longOpt(REPL_OPTION)
      .desc("Runs the Enso REPL.")
      .build
    val run = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("file")
      .longOpt(RUN_OPTION)
      .desc("Runs a specified Enso file.")
      .build
    val newOpt = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("path")
      .longOpt(NEW_OPTION)
      .desc("Creates a new Enso project.")
      .build
    val jupyterOption = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("connection file")
      .longOpt(JUPYTER_OPTION)
      .desc("Runs Enso Jupyter Kernel.")
      .build
    val lsOption = CliOption.builder
      .longOpt(LANGUAGE_SERVER_OPTION)
      .desc("Runs Language Server")
      .build()
    val interfaceOption = CliOption.builder
      .longOpt(INTERFACE_OPTION)
      .hasArg(true)
      .numberOfArgs(1)
      .argName("interface")
      .desc("Interface for processing all incoming connections")
      .build()
    val portOption = CliOption.builder
      .longOpt(PORT_OPTION)
      .hasArg(true)
      .numberOfArgs(1)
      .argName("port")
      .desc("Port for processing all incoming connections")
      .build()
    val uuidOption = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("uuid")
      .longOpt(ROOT_ID_OPTION)
      .desc("Content root id.")
      .build()
    val pathOption = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("path")
      .longOpt(ROOT_PATH_OPTION)
      .desc("Path to the content root.")
      .build()
    val version = CliOption.builder
      .longOpt(VERSION_OPTION)
      .desc("Checks the version of the Enso executable.")
      .build

    val options = new Options
    options
      .addOption(help)
      .addOption(repl)
      .addOption(run)
      .addOption(newOpt)
      .addOption(jupyterOption)
      .addOption(lsOption)
      .addOption(interfaceOption)
      .addOption(portOption)
      .addOption(uuidOption)
      .addOption(pathOption)
      .addOption(version)

    options
  }

  /**
    * Prints the help message to the standard output.
    *
    * @param options object representing the CLI syntax
    */
  private def printHelp(options: Options): Unit =
    new HelpFormatter().printHelp(LanguageInfo.ID, options)

  /** Terminates the process with a failure exit code. */
  private def exitFail(): Unit = System.exit(1)

  /** Terminates the process with a success exit code. */
  private def exitSuccess(): Unit = System.exit(0)

  /**
    * Handles the `--new` CLI option.
    *
    * @param path root path of the newly created project
    */
  private def createNew(path: String): Unit = {
    Package.getOrCreate(new File(path))
    exitSuccess()
  }

  /**
    * Handles the `--run` CLI option.
    *
    * @param path path of the project or file to execute
    */
  private def run(path: String): Unit = {
    val file = new File(path)
    if (!file.exists) {
      println(s"File $file does not exist.")
      exitFail()
    }
    val projectMode = file.isDirectory
    val packagePath =
      if (projectMode) file.getAbsolutePath
      else ""
    val context = new ContextFactory().create(
      packagePath,
      System.in,
      System.out,
      Repl(TerminalIO())
    )
    if (projectMode) {
      val pkg  = Package.fromDirectory(file)
      val main = pkg.map(_.mainFile)
      if (!main.exists(_.exists())) {
        println("Main file does not exist.")
        exitFail()
      }
      val mainFile       = main.get
      val mainModuleName = pkg.get.moduleNameForFile(mainFile).toString
      runPackage(context, mainModuleName)
    } else {
      runSingleFile(context, file)
    }
    exitSuccess()
  }

  private def runPackage(
    context: PolyglotContext,
    mainModuleName: String
  ): Unit = {
    val topScope   = context.getTopScope
    val mainModule = topScope.getModule(mainModuleName)
    runMain(mainModule): Unit
  }

  private def runSingleFile(context: PolyglotContext, file: File): Unit = {
    val mainModule = context.evalModule(file)
    runMain(mainModule): Unit
  }

  private def runMain(mainModule: Module): Value = {
    val mainCons = mainModule.getAssociatedConstructor
    val mainFun  = mainModule.getMethod(mainCons, "main")
    mainFun.execute(mainCons.newInstance())
  }

  /**
    * Handles the `--repl` CLI option
    */
  private def runRepl(): Unit = {
    val dummySourceToTriggerRepl = "main = Debug.breakpoint"
    val replModuleName           = "Repl"
    val context =
      new ContextFactory().create("", System.in, System.out, Repl(TerminalIO()))
    val mainModule =
      context.evalModule(dummySourceToTriggerRepl, replModuleName)
    runMain(mainModule)
    exitSuccess()
  }

  /**
    * Handles `--server` CLI option
    *
    * @param line a CLI line
    */
  private def runLanguageServer(line: CommandLine): Unit = {
    val maybeConfig = parseSeverOptions(line)

    maybeConfig match {
      case Left(errorMsg) =>
        System.err.println(errorMsg)
        exitFail()

      case Right(config) =>
        LanguageServerApp.run(config)
        exitSuccess()
    }
  }

  private def parseSeverOptions(
    line: CommandLine
  ): Either[String, LanguageServerConfig] =
    // format: off
    for {
      rootId    <- Option(line.getOptionValue(ROOT_ID_OPTION))
                     .toRight("Root id must be provided")
                     .flatMap { id =>
                       Either
                         .catchNonFatal(UUID.fromString(id))
                         .leftMap(_ => "Root must be UUID")
                     }
      rootPath  <- Option(line.getOptionValue(ROOT_PATH_OPTION))
                     .toRight("Root path must be provided")
      interface  = Option(line.getOptionValue(INTERFACE_OPTION))
                     .getOrElse("127.0.0.1")
      portString = Option(line.getOptionValue(PORT_OPTION)).getOrElse("8080")
      port      <- Either
                     .catchNonFatal(portString.toInt)
                     .leftMap(_ => "Port must be integer") 
    } yield languageserver.LanguageServerConfig(interface, port, rootId, rootPath)
    // format: on

  /** Prints the version of the enso executable.
    *
    * @param options A description of the CLI options syntax
    */
  def displayVersion(@unused options: Options): Unit = {
    // Running platform information
    val vmName     = System.getProperty("java.vm.name")
    val jreVersion = System.getProperty("java.runtime.version")
    val osArch     = System.getProperty("os.arch")
    val osName     = System.getProperty("os.name")
    val osVersion  = System.getProperty("os.version")

    val dirtyStr = if (Info.isDirty) {
      "*"
    } else {
      ""
    }

    val versionOutput =
      s"""
         |Enso Compiler and Runtime
         |Version:    ${Info.ensoVersion}
         |Built with: scala-${Info.scalacVersion} for GraalVM ${Info.graalVersion}
         |Built from: ${Info.branch}$dirtyStr @ ${Info.commit}
         |Running on: $vmName, JDK $jreVersion
         |            $osName $osVersion ($osArch)
         |""".stripMargin

    println(versionOutput)
  }

  /**
    * Main entry point for the CLI program.
    *
    * @param args the command line arguments
    */
  def main(args: Array[String]): Unit = {
    val options = buildOptions
    val parser  = new DefaultParser
    val line: CommandLine = Try(parser.parse(options, args)).getOrElse {
      printHelp(options)
      exitFail()
      return
    }
    if (line.hasOption(HELP_OPTION)) {
      printHelp(options)
      exitSuccess()
    }
    if (line.hasOption(VERSION_OPTION)) {
      displayVersion(options)
      exitSuccess()
    }
    if (line.hasOption(NEW_OPTION)) {
      createNew(line.getOptionValue(NEW_OPTION))
    }
    if (line.hasOption(RUN_OPTION)) {
      run(line.getOptionValue(RUN_OPTION))
    }
    if (line.hasOption(REPL_OPTION)) {
      runRepl()
    }
    if (line.hasOption(JUPYTER_OPTION)) {
      new JupyterKernel().run(line.getOptionValue(JUPYTER_OPTION))
    }
    if (line.hasOption(LANGUAGE_SERVER_OPTION)) {
      runLanguageServer(line)
    }
    printHelp(options)
    exitFail()
  }
}
