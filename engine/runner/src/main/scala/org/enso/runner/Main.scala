package org.enso.runner

import java.io.File
import java.util.UUID

import akka.http.scaladsl.model.{IllegalUriException, Uri}
import cats.implicits._
import nl.gn0s1s.bump.SemVer
import org.apache.commons.cli.{Option => CliOption, _}
import org.enso.languageserver.boot
import org.enso.languageserver.boot.LanguageServerConfig
import org.enso.loggingservice.LogLevel
import org.enso.pkg.{Contact, PackageManager, SemVerEnsoVersion}
import org.enso.polyglot.{LanguageInfo, Module, PolyglotContext}
import org.enso.version.VersionDescription
import org.graalvm.polyglot.{PolyglotException, Value}

import scala.jdk.CollectionConverters._
import scala.util.Try

/** The main CLI entry point class. */
object Main {

  private val RUN_OPTION                  = "run"
  private val HELP_OPTION                 = "help"
  private val NEW_OPTION                  = "new"
  private val PROJECT_NAME_OPTION         = "new-project-name"
  private val PROJECT_AUTHOR_NAME_OPTION  = "new-project-author-name"
  private val PROJECT_AUTHOR_EMAIL_OPTION = "new-project-author-email"
  private val REPL_OPTION                 = "repl"
  private val LANGUAGE_SERVER_OPTION      = "server"
  private val INTERFACE_OPTION            = "interface"
  private val RPC_PORT_OPTION             = "rpc-port"
  private val DATA_PORT_OPTION            = "data-port"
  private val ROOT_ID_OPTION              = "root-id"
  private val ROOT_PATH_OPTION            = "path"
  private val IN_PROJECT_OPTION           = "in-project"
  private val VERSION_OPTION              = "version"
  private val JSON_OPTION                 = "json"
  private val LOG_LEVEL                   = "log-level"
  private val LOGGER_CONNECT              = "logger-connect"

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
    val newProjectNameOpt = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("name")
      .longOpt(PROJECT_NAME_OPTION)
      .desc(
        s"Specifies a project name when creating a project using --$NEW_OPTION."
      )
      .build
    val newProjectAuthorNameOpt = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("name")
      .longOpt(PROJECT_AUTHOR_NAME_OPTION)
      .desc(
        s"Specifies the name of the author and maintainer of the project " +
        s"created using --$NEW_OPTION."
      )
      .build
    val newProjectAuthorEmailOpt = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("email")
      .longOpt(PROJECT_AUTHOR_EMAIL_OPTION)
      .desc(
        s"Specifies the email of the author and maintainer of the project " +
        s"created using --$NEW_OPTION."
      )
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
    val rpcPortOption = CliOption.builder
      .longOpt(RPC_PORT_OPTION)
      .hasArg(true)
      .numberOfArgs(1)
      .argName("rpc-port")
      .desc("RPC port for processing all incoming connections")
      .build()
    val dataPortOption = CliOption.builder
      .longOpt(DATA_PORT_OPTION)
      .hasArg(true)
      .numberOfArgs(1)
      .argName("data-port")
      .desc("Data port for visualisation protocol")
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
    val inProjectOption = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("project-path")
      .longOpt(IN_PROJECT_OPTION)
      .desc(
        "Setting this option when running the REPL or an Enso script, runs it" +
        "in context of the specified project."
      )
      .build()
    val version = CliOption.builder
      .longOpt(VERSION_OPTION)
      .desc("Checks the version of the Enso executable.")
      .build
    val json = CliOption.builder
      .longOpt(JSON_OPTION)
      .desc("Switches the --version option to JSON output.")
      .build
    val logLevelOption = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("log-level")
      .longOpt(LOG_LEVEL)
      .desc(
        "Sets the runtime log level. Possible values are: OFF, ERROR, " +
        "WARNING, INFO, DEBUG and TRACE. Default: INFO."
      )
      .build
    val loggerConnectOption = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("uri")
      .longOpt(LOGGER_CONNECT)
      .desc("Connects to a logging service server and passes all logs to it.")
      .build

    val options = new Options
    options
      .addOption(help)
      .addOption(repl)
      .addOption(run)
      .addOption(newOpt)
      .addOption(newProjectNameOpt)
      .addOption(newProjectAuthorNameOpt)
      .addOption(newProjectAuthorEmailOpt)
      .addOption(lsOption)
      .addOption(interfaceOption)
      .addOption(rpcPortOption)
      .addOption(dataPortOption)
      .addOption(uuidOption)
      .addOption(pathOption)
      .addOption(inProjectOption)
      .addOption(version)
      .addOption(json)
      .addOption(logLevelOption)
      .addOption(loggerConnectOption)

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
  private def exitFail(): Nothing = exit(1)

  /** Terminates the process with a success exit code. */
  private def exitSuccess(): Nothing = exit(0)

  /** Shuts down the logging service and terminates the process. */
  private def exit(exitCode: Int): Nothing = {
    RunnerLogging.tearDown()
    sys.exit(exitCode)
  }

  /**
    * Handles the `--new` CLI option.
    *
    * Creates a project at the provided path. If the nameOption is provided it
    * specifies the project name, otherwise the name is generated automatically.
    * The Enso version used in the project is set to the version of this runner.
    *
    * @param path root path of the newly created project
    * @param nameOption specifies the name of the created project
    * @param authorName if set, sets the name of the author and maintainer
    * @param authorEmail if set, sets the email of the author and maintainer
    */
  private def createNew(
    path: String,
    nameOption: Option[String],
    authorName: Option[String],
    authorEmail: Option[String]
  ): Unit = {
    val root = new File(path)
    val name = nameOption.getOrElse(PackageManager.Default.generateName(root))
    val currentVersion = SemVer(buildinfo.Info.ensoVersion).getOrElse {
      throw new IllegalStateException(
        "Fatal error: Enso version included in buildinfo is not a valid " +
        "semver string, this should never happen."
      )
    }
    val authors =
      if (authorName.isEmpty && authorEmail.isEmpty) List()
      else List(Contact(name = authorName, email = authorEmail))
    PackageManager.Default.create(
      root        = root,
      name        = name,
      ensoVersion = SemVerEnsoVersion(currentVersion),
      authors     = authors,
      maintainers = authors
    )
    exitSuccess()
  }

  /**
    * Handles the `--run` CLI option.
    *
    * If `path` is a directory, so a project is run, a conflicting (pointing to
    * another project) `projectPath` should not be provided.
    *
    * @param path path of the project or file to execute
    * @param projectPath if specified, the script is run in context of a
    *                    project located at that path
    * @param logLevel log level to set for the engine runtime
    */
  private def run(
    path: String,
    projectPath: Option[String],
    logLevel: LogLevel
  ): Unit = {
    val file = new File(path)
    if (!file.exists) {
      println(s"File $file does not exist.")
      exitFail()
    }
    val projectMode = file.isDirectory
    val packagePath =
      if (projectMode) {
        projectPath match {
          case Some(inProject) if inProject != path =>
            println(
              "It is not possible to run a project in context of another " +
              "project, please do not use the `--in-project` option for " +
              "running projects."
            )
            exitFail()
          case _ =>
        }
        file.getAbsolutePath
      } else projectPath.getOrElse("")
    val context = new ContextFactory().create(
      packagePath,
      System.in,
      System.out,
      Repl(TerminalIO()),
      strictErrors = true,
      logLevel     = logLevel
    )
    if (projectMode) {
      val pkg  = PackageManager.Default.fromDirectory(file)
      val main = pkg.map(_.mainFile)
      if (!main.exists(_.exists())) {
        println("Main file does not exist.")
        exitFail()
      }
      val mainFile       = main.get
      val mainModuleName = pkg.get.moduleNameForFile(mainFile).toString
      runPackage(context, mainModuleName, file)
    } else {
      runSingleFile(context, file)
    }
    exitSuccess()
  }

  private def runPackage(
    context: PolyglotContext,
    mainModuleName: String,
    projectPath: File
  ): Unit = {
    val topScope   = context.getTopScope
    val mainModule = topScope.getModule(mainModuleName)
    runMain(mainModule, Some(projectPath))
  }

  private def runSingleFile(context: PolyglotContext, file: File): Unit = {
    val mainModule = context.evalModule(file)
    runMain(mainModule, Some(file))
  }

  private def printPolyglotException(
    exception: PolyglotException,
    relativeTo: Option[File]
  ): Unit = {
    val fullStack = exception.getPolyglotStackTrace.asScala.toList
    val dropInitJava = fullStack.reverse
      .dropWhile(_.getLanguage.getId != LanguageInfo.ID)
      .reverse
    println(s"Execution finished with an error: ${exception.getMessage}")
    dropInitJava.foreach { frame =>
      val langId =
        if (frame.isHostFrame) "java" else frame.getLanguage.getId
      val fmtFrame = if (frame.getLanguage.getId == LanguageInfo.ID) {
        val fName = frame.getRootName
        val src = Option(frame.getSourceLocation)
          .map { sourceLoc =>
            val ident = Option(sourceLoc.getSource.getPath)
              .map { path =>
                relativeTo match {
                  case None => path
                  case Some(root) =>
                    val absRoot = root.getAbsoluteFile
                    if (path.startsWith(absRoot.getAbsolutePath)) {
                      val rootDir =
                        if (absRoot.isDirectory) absRoot
                        else absRoot.getParentFile
                      rootDir.toPath.relativize(new File(path).toPath).toString
                    } else {
                      path
                    }
                }
              }
              .getOrElse(sourceLoc.getSource.getName)
            val loc = if (sourceLoc.getStartLine == sourceLoc.getEndLine) {
              val line  = sourceLoc.getStartLine
              val start = sourceLoc.getStartColumn
              val end   = sourceLoc.getEndColumn
              s"$line:$start-$end"
            } else {
              s"${sourceLoc.getStartLine}-${sourceLoc.getEndLine}"
            }
            s"$ident:$loc"
          }
          .getOrElse("Internal")
        s"$fName($src)"
      } else {
        frame.toString
      }
      println(s"        at <$langId> $fmtFrame")
    }
  }

  private def runMain(
    mainModule: Module,
    rootPkgPath: Option[File],
    mainMethodName: String = "main"
  ): Value = {
    val mainCons = mainModule.getAssociatedConstructor
    val mainFun  = mainModule.getMethod(mainCons, mainMethodName)
    try {
      mainFun.execute(mainCons.newInstance())
    } catch {
      case e: PolyglotException =>
        printPolyglotException(e, rootPkgPath)
        exitFail()
    }
  }

  /**
    * Handles the `--repl` CLI option
    *
    * @param projectPath if specified, the REPL is run in context of a project
    *                    at the given path
    * @param logLevel log level to set for the engine runtime
    */
  private def runRepl(projectPath: Option[String], logLevel: LogLevel): Unit = {
    val mainMethodName = "internal_repl_entry_point___"
    // TODO[MK, RW]: when CI-testing can use a fully-built distribution,
    // switch to `from Base import all` here.
    val dummySourceToTriggerRepl =
      s"""from Builtins import all
         |
         |$mainMethodName = Debug.breakpoint
         |""".stripMargin
    val replModuleName = "Internal_Repl_Module___"
    val packagePath    = projectPath.getOrElse("")
    val context =
      new ContextFactory().create(
        packagePath,
        System.in,
        System.out,
        Repl(TerminalIO()),
        logLevel = logLevel
      )
    val mainModule =
      context.evalModule(dummySourceToTriggerRepl, replModuleName)
    runMain(mainModule, None, mainMethodName = mainMethodName)
    exitSuccess()
  }

  /**
    * Handles `--server` CLI option
    *
    * @param line a CLI line
    * @param logLevel log level to set for the engine runtime
    */
  private def runLanguageServer(line: CommandLine, logLevel: LogLevel): Unit = {
    val _ = logLevel // TODO [RW] handle logging in the Language Server (#1144)

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
      rootId     <- Option(line.getOptionValue(ROOT_ID_OPTION))
                      .toRight("Root id must be provided")
                      .flatMap { id =>
                        Either
                          .catchNonFatal(UUID.fromString(id))
                          .leftMap(_ => "Root must be UUID")
                      }
      rootPath   <- Option(line.getOptionValue(ROOT_PATH_OPTION))
                      .toRight("Root path must be provided")
      interface   = Option(line.getOptionValue(INTERFACE_OPTION))
                      .getOrElse("127.0.0.1")
      rpcPortStr  = Option(line.getOptionValue(RPC_PORT_OPTION)).getOrElse("8080")
      rpcPort    <- Either
                      .catchNonFatal(rpcPortStr.toInt)
                      .leftMap(_ => "Port must be integer")
      dataPortStr = Option(line.getOptionValue(DATA_PORT_OPTION)).getOrElse("8081")
      dataPort   <- Either
                      .catchNonFatal(dataPortStr.toInt)
                      .leftMap(_ => "Port must be integer")
    } yield boot.LanguageServerConfig(interface, rpcPort, dataPort, rootId, rootPath)
    // format: on

  /** Prints the version of the Enso executable.
    *
    * @param useJson whether the output should be JSON or human-readable.
    */
  def displayVersion(useJson: Boolean): Unit = {
    val versionDescription = VersionDescription.make(
      "Enso Compiler and Runtime",
      includeRuntimeJVMInfo = true
    )
    println(versionDescription.asString(useJson))
  }

  /**
    * Parses the log level option.
    */
  def parseLogLevel(levelOption: String): LogLevel = {
    val name = levelOption.toLowerCase
    LogLevel.allLevels.find(_.toString.toLowerCase == name).getOrElse {
      val possible =
        LogLevel.allLevels.map(_.toString.toLowerCase).mkString(", ")
      System.err.println(s"Invalid log level. Possible values are $possible.")
      exitFail()
    }
  }

  /**
    * Parses an URI that specifies the logging service connection.
    */
  def parseUri(string: String): Uri =
    try {
      Uri(string)
    } catch {
      case _: IllegalUriException =>
        System.err.println(s"`$string` is not a valid URI.")
        exitFail()
    }

  /**
    * Default log level to use if the LOG_LEVEL option is not provided.
    */
  val defaultLogLevel: LogLevel = LogLevel.Info

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
    }
    if (line.hasOption(HELP_OPTION)) {
      printHelp(options)
      exitSuccess()
    }
    if (line.hasOption(VERSION_OPTION)) {
      displayVersion(useJson = line.hasOption(JSON_OPTION))
      exitSuccess()
    }

    val logLevel = Option(line.getOptionValue(LOG_LEVEL))
      .map(parseLogLevel)
      .getOrElse(defaultLogLevel)
    val connectionUri =
      Option(line.getOptionValue(LOGGER_CONNECT)).map(parseUri)
    RunnerLogging.setup(connectionUri, logLevel)

    if (line.hasOption(NEW_OPTION)) {
      createNew(
        path        = line.getOptionValue(NEW_OPTION),
        nameOption  = Option(line.getOptionValue(PROJECT_NAME_OPTION)),
        authorName  = Option(line.getOptionValue(PROJECT_AUTHOR_NAME_OPTION)),
        authorEmail = Option(line.getOptionValue(PROJECT_AUTHOR_EMAIL_OPTION))
      )
    }

    if (line.hasOption(RUN_OPTION)) {
      run(
        line.getOptionValue(RUN_OPTION),
        Option(line.getOptionValue(IN_PROJECT_OPTION)),
        logLevel
      )
    }
    if (line.hasOption(REPL_OPTION)) {
      runRepl(Option(line.getOptionValue(IN_PROJECT_OPTION)), logLevel)
    }
    if (line.hasOption(LANGUAGE_SERVER_OPTION)) {
      runLanguageServer(line, logLevel)
    }
    printHelp(options)
    exitFail()
  }
}
