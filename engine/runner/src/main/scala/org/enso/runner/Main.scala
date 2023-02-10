package org.enso.runner

import akka.http.scaladsl.model.{IllegalUriException, Uri}
import buildinfo.Info
import cats.implicits._
import com.typesafe.scalalogging.Logger
import org.apache.commons.cli.{Option => CliOption, _}
import org.enso.editions.DefaultEdition
import org.enso.languageserver.boot
import org.enso.languageserver.boot.{LanguageServerConfig, ProfilingConfig}
import org.enso.libraryupload.LibraryUploader.UploadFailedError
import org.enso.loggingservice.LogLevel
import org.enso.pkg.{Contact, PackageManager, Template}
import org.enso.polyglot.{LanguageInfo, Module, PolyglotContext}
import org.enso.version.VersionDescription
import org.graalvm.polyglot.PolyglotException

import java.io.File
import java.nio.file.{Path, Paths}
import java.util.{HashMap, UUID}
import scala.Console.err
import scala.Console.out
import scala.collection.compat.immutable.ArraySeq
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

/** The main CLI entry point class. */
object Main {

  private val RUN_OPTION                     = "run"
  private val INSPECT_OPTION                 = "inspect"
  private val DUMP_GRAPHS_OPTION             = "dump-graphs"
  private val HELP_OPTION                    = "help"
  private val NEW_OPTION                     = "new"
  private val PROJECT_NAME_OPTION            = "new-project-name"
  private val PROJECT_TEMPLATE_OPTION        = "new-project-template"
  private val PROJECT_AUTHOR_NAME_OPTION     = "new-project-author-name"
  private val PROJECT_AUTHOR_EMAIL_OPTION    = "new-project-author-email"
  private val REPL_OPTION                    = "repl"
  private val DOCS_OPTION                    = "docs"
  private val PREINSTALL_OPTION              = "preinstall-dependencies"
  private val LANGUAGE_SERVER_OPTION         = "server"
  private val LANGUAGE_SERVER_PROFILING_PATH = "server-profiling-path"
  private val LANGUAGE_SERVER_PROFILING_TIME = "server-profiling-time"
  private val LANGUAGE_SERVER_PROFILING_EVENTS_LOG_PATH =
    "server-profiling-events-log-path"
  private val DAEMONIZE_OPTION               = "daemon"
  private val INTERFACE_OPTION               = "interface"
  private val RPC_PORT_OPTION                = "rpc-port"
  private val DATA_PORT_OPTION               = "data-port"
  private val ROOT_ID_OPTION                 = "root-id"
  private val ROOT_PATH_OPTION               = "path"
  private val IN_PROJECT_OPTION              = "in-project"
  private val VERSION_OPTION                 = "version"
  private val JSON_OPTION                    = "json"
  private val IR_CACHES_OPTION               = "ir-caches"
  private val NO_IR_CACHES_OPTION            = "no-ir-caches"
  private val NO_READ_IR_CACHES_OPTION       = "no-read-ir-caches"
  private val COMPILE_OPTION                 = "compile"
  private val NO_COMPILE_DEPENDENCIES_OPTION = "no-compile-dependencies"
  private val NO_GLOBAL_CACHE_OPTION         = "no-global-cache"
  private val LOG_LEVEL                      = "log-level"
  private val LOGGER_CONNECT                 = "logger-connect"
  private val NO_LOG_MASKING                 = "no-log-masking"
  private val UPLOAD_OPTION                  = "upload"
  private val UPDATE_MANIFEST_OPTION         = "update-manifest"
  private val HIDE_PROGRESS                  = "hide-progress"
  private val AUTH_TOKEN                     = "auth-token"
  private val AUTO_PARALLELISM_OPTION        = "with-auto-parallelism"

  private lazy val logger = Logger[Main.type]

  private def isDevBuild: Boolean = {
    Info.ensoVersion.matches(".+-SNAPSHOT$")
  }

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
    val inspect = CliOption.builder
      .longOpt(INSPECT_OPTION)
      .desc("Start the Chrome inspector when --run is used.")
      .build
    val dumpGraphs = CliOption.builder
      .longOpt(DUMP_GRAPHS_OPTION)
      .desc("Dumps IGV graphs when --run is used.")
      .build
    val docs = CliOption.builder
      .longOpt(DOCS_OPTION)
      .desc("Runs the Enso documentation generator.")
      .build
    val preinstall = CliOption.builder
      .longOpt(PREINSTALL_OPTION)
      .desc("Installs dependencies of the project.")
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
    val newProjectTemplateOpt = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("name")
      .longOpt(PROJECT_TEMPLATE_OPTION)
      .desc(
        s"Specifies a project template when creating a project using --$NEW_OPTION."
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
    val lsProfilingPathOption = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("file")
      .longOpt(LANGUAGE_SERVER_PROFILING_PATH)
      .desc("The path to the Language Server profiling file.")
      .build()
    val lsProfilingTimeOption = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("seconds")
      .longOpt(LANGUAGE_SERVER_PROFILING_TIME)
      .desc("The duration in seconds limiting the profiling time.")
      .build()
    val lsProfilingEventsLogPathOption = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("file")
      .longOpt(LANGUAGE_SERVER_PROFILING_EVENTS_LOG_PATH)
      .desc("The path to the runtime events log file.")
      .build()
    val deamonizeOption = CliOption.builder
      .longOpt(DAEMONIZE_OPTION)
      .desc("Deamonize Language Server")
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
    val noLogMaskingOption: CliOption = CliOption.builder
      .longOpt(NO_LOG_MASKING)
      .desc(
        "Disable masking of personally identifiable information in logs. " +
        "Masking can be also disabled with the `NO_LOG_MASKING` environment " +
        "variable."
      )
      .build()
    val uploadOption = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("url")
      .longOpt(UPLOAD_OPTION)
      .desc(
        "Uploads the library to a repository. " +
        "The url defines the repository to upload to."
      )
      .build()
    val updateManifestOption = CliOption.builder
      .longOpt(UPDATE_MANIFEST_OPTION)
      .desc(
        "Updates the library manifest with the updated list of direct " +
        "dependencies."
      )
      .build()
    val hideProgressOption = CliOption.builder
      .longOpt(HIDE_PROGRESS)
      .desc("If specified, progress bars will not be displayed.")
      .build()
    val authTokenOption = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("token")
      .longOpt(AUTH_TOKEN)
      .desc("Authentication token for the upload.")
      .build()
    val noReadIrCachesOption = CliOption.builder
      .longOpt(NO_READ_IR_CACHES_OPTION)
      .desc(
        "Disables the reading of IR caches in the runtime if IR caching is enabled."
      )
      .build()
    val compileOption = CliOption.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("package")
      .longOpt(COMPILE_OPTION)
      .desc("Compile the provided package without executing it.")
      .build()
    val noCompileDependenciesOption = CliOption.builder
      .longOpt(NO_COMPILE_DEPENDENCIES_OPTION)
      .desc(
        "Tells the compiler to not compile dependencies when performing static compilation."
      )
      .build()
    val noGlobalCacheOption = CliOption.builder
      .longOpt(NO_GLOBAL_CACHE_OPTION)
      .desc(
        "Tells the compiler not to write compiled data to the global cache locations."
      )
      .build()

    val irCachesOption = CliOption.builder
      .longOpt(IR_CACHES_OPTION)
      .desc(
        "Enables IR caches. These are on by default in production builds " +
        "and off by default in developer builds. You may not specify this " +
        "option with `--no-ir-caches`."
      )
      .build()
    val noIrCachesOption = CliOption.builder
      .longOpt(NO_IR_CACHES_OPTION)
      .desc(
        "Disables IR caches. These are on by default in production builds " +
        "and off by default in developer builds. You may not specify this " +
        "option with `--ir-caches`."
      )
      .build()

    val cacheOptionsGroup = new OptionGroup
    cacheOptionsGroup.addOption(irCachesOption)
    cacheOptionsGroup.addOption(noIrCachesOption)

    val autoParallelism = CliOption.builder
      .longOpt(AUTO_PARALLELISM_OPTION)
      .desc("Enables auto parallelism in the Enso interpreter.")
      .build

    val options = new Options
    options
      .addOption(help)
      .addOption(repl)
      .addOption(run)
      .addOption(inspect)
      .addOption(dumpGraphs)
      .addOption(docs)
      .addOption(preinstall)
      .addOption(newOpt)
      .addOption(newProjectNameOpt)
      .addOption(newProjectTemplateOpt)
      .addOption(newProjectAuthorNameOpt)
      .addOption(newProjectAuthorEmailOpt)
      .addOption(lsOption)
      .addOption(lsProfilingPathOption)
      .addOption(lsProfilingTimeOption)
      .addOption(lsProfilingEventsLogPathOption)
      .addOption(deamonizeOption)
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
      .addOption(noLogMaskingOption)
      .addOption(uploadOption)
      .addOption(updateManifestOption)
      .addOption(hideProgressOption)
      .addOption(authTokenOption)
      .addOption(noReadIrCachesOption)
      .addOption(compileOption)
      .addOption(noCompileDependenciesOption)
      .addOption(noGlobalCacheOption)
      .addOptionGroup(cacheOptionsGroup)
      .addOption(autoParallelism)

    options
  }

  /** Prints the help message to the standard output.
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

  /** Handles the `--new` CLI option.
    *
    * Creates a project at the provided path. If the nameOption is provided it
    * specifies the project name, otherwise the name is generated automatically.
    * The Enso version used in the project is set to the version of this runner.
    *
    * @param path           root path of the newly created project
    * @param nameOption     specifies the name of the created project
    * @param templateOption specifies the template of the created project
    * @param authorName     if set, sets the name of the author and maintainer
    * @param authorEmail    if set, sets the email of the author and maintainer
    */
  private def createNew(
    path: String,
    nameOption: Option[String],
    templateOption: Option[String],
    authorName: Option[String],
    authorEmail: Option[String]
  ): Unit = {
    val root = new File(path)
    val name = nameOption.getOrElse(PackageManager.Default.generateName(root))
    val authors =
      if (authorName.isEmpty && authorEmail.isEmpty) List()
      else List(Contact(name = authorName, email = authorEmail))

    val edition = DefaultEdition.getDefaultEdition
    logger.whenTraceEnabled {
      val baseEdition = edition.parent.getOrElse("<no-base>")
      logger.trace(
        s"Creating a new project $name based on edition [$baseEdition]."
      )
    }

    val template = templateOption
      .map { name =>
        Template.fromString(name).getOrElse {
          logger.error(s"Unknown project template name: '$name'.")
          exitFail()
        }
      }

    PackageManager.Default.create(
      root        = root,
      name        = name,
      edition     = Some(edition),
      authors     = authors,
      maintainers = authors,
      template    = template.getOrElse(Template.Default)
    )
    exitSuccess()
  }

  /** Handles the `--compile` CLI option.
    *
    * @param packagePath the path to the package being compiled
    * @param shouldCompileDependencies whether the dependencies of that package
    *                                  should also be compiled
    * @param shouldUseGlobalCache whether or not the compilation result should
    *                             be written to the global cache
    * @param logLevel the logging level
    * @param logMasking whether or not log masking is enabled
    */
  private def compile(
    packagePath: String,
    shouldCompileDependencies: Boolean,
    shouldUseGlobalCache: Boolean,
    logLevel: LogLevel,
    logMasking: Boolean
  ): Unit = {
    val file = new File(packagePath)
    if (!file.exists || !file.isDirectory) {
      println(s"No package exists at $file.")
      exitFail()
    }

    val context = new ContextFactory().create(
      packagePath,
      System.in,
      System.out,
      Repl(TerminalIO()),
      logLevel,
      logMasking,
      enableIrCaches           = true,
      useGlobalIrCacheLocation = shouldUseGlobalCache
    )
    val topScope = context.getTopScope
    topScope.compile(shouldCompileDependencies)

    context.context.close()
    exitSuccess()
  }

  /** Handles the `--run` CLI option.
    *
    * If `path` is a directory, so a project is run, a conflicting (pointing to
    * another project) `projectPath` should not be provided.
    *
    * @param path           path of the project or file to execute
    * @param projectPath    if specified, the script is run in context of a
    *                       project located at that path
    * @param logLevel       log level to set for the engine runtime
    * @param logMasking     is the log masking enabled
    * @param enableIrCaches are IR caches enabled
    * @param inspect        shall inspect option be enabled
    * @param dump           shall graphs be sent to the IGV
    */
  private def run(
    path: String,
    additionalArgs: Array[String],
    projectPath: Option[String],
    logLevel: LogLevel,
    logMasking: Boolean,
    enableIrCaches: Boolean,
    enableAutoParallelism: Boolean,
    inspect: Boolean,
    dump: Boolean
  ): Unit = {
    val file = new File(path)
    if (!file.exists) {
      println(s"File $file does not exist.")
      exitFail()
    }
    val projectMode = file.isDirectory
    val projectRoot =
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
    val options = new HashMap[String, String]()
    if (dump) {
      options.put("engine.TraceCompilation", "true")
      options.put("engine.MultiTier", "false")
    }
    if (inspect) {
      options.put("inspect", "")
    }
    val context = new ContextFactory().create(
      projectRoot,
      System.in,
      System.out,
      Repl(TerminalIO()),
      logLevel,
      logMasking,
      enableIrCaches,
      strictErrors          = true,
      enableAutoParallelism = enableAutoParallelism,
      options               = options
    )
    if (projectMode) {
      PackageManager.Default.loadPackage(file) match {
        case Success(pkg) =>
          val main = pkg.mainFile
          if (!main.exists()) {
            println("Main file does not exist.")
            context.context.close()
            exitFail()
          }
          val mainModuleName = pkg.moduleNameForFile(pkg.mainFile).toString
          runPackage(context, mainModuleName, file, additionalArgs)
        case Failure(ex) =>
          println(ex.getMessage)
          exitFail()
      }
    } else {
      runSingleFile(context, file, additionalArgs)
    }
    context.context.close()
    exitSuccess()
  }

  /** Handles the `--docs` CLI option.
    *
    * Generates reference website from standard library.
    *
    * @param projectPath if specified, the docs is generated for a project
    *                    at the given path
    * @param logLevel    log level to set for the engine runtime
    * @param logMasking  is the log masking enabled
    * @param enableIrCaches are the IR caches enabled
    */
  private def genDocs(
    projectPath: Option[String],
    logLevel: LogLevel,
    logMasking: Boolean,
    enableIrCaches: Boolean
  ): Unit = {
    if (projectPath.isEmpty) {
      println("Path hasn't been provided.")
      exitFail()
    }
    generateDocsFrom(
      projectPath.get,
      logLevel,
      logMasking,
      enableIrCaches
    )
    exitSuccess()
  }

  /** Subroutine of `genDocs` function.
    * Generates the documentation for given Enso project at given path.
    */
  private def generateDocsFrom(
    path: String,
    logLevel: LogLevel,
    logMasking: Boolean,
    enableIrCaches: Boolean
  ): Unit = {
    val executionContext = new ContextFactory().create(
      path,
      System.in,
      System.out,
      Repl(TerminalIO()),
      logLevel,
      logMasking,
      enableIrCaches
    )

    val file = new File(path)
    val pkg  = PackageManager.Default.fromDirectory(file)
    val main = pkg.map(_.mainFile)

    if (main.exists(_.exists())) {
      val mainFile       = main.get
      val mainModuleName = pkg.get.moduleNameForFile(mainFile).toString
      val topScope       = executionContext.getTopScope
      val mainModule     = topScope.getModule(mainModuleName)
      val generated      = mainModule.generateDocs()
      print(generated)

      // TODO:
      // - go through executed code and get all HTML docs
      //   with their corresponding atoms/methods etc.
      // - Save those to files
    }
  }

  /** Handles the `--preinstall-dependencies` CLI option.
    *
    * Gathers imported dependencies and ensures that all of them are installed.
    *
    * @param projectPath path of the project
    * @param logLevel log level to set for the engine runtime
    */
  private def preinstallDependencies(
    projectPath: Option[String],
    logLevel: LogLevel
  ): Unit = projectPath match {
    case Some(path) =>
      try {
        DependencyPreinstaller.preinstallDependencies(new File(path), logLevel)
        exitSuccess()
      } catch {
        case NonFatal(error) =>
          logger.error(
            s"Dependency installation failed: ${error.getMessage}",
            error
          )
          exitFail()
      }
    case None =>
      println("Dependency installation is only available for projects.")
      exitFail()
  }

  private def runPackage(
    context: PolyglotContext,
    mainModuleName: String,
    projectPath: File,
    additionalArgs: Array[String]
  ): Unit = {
    val topScope   = context.getTopScope
    val mainModule = topScope.getModule(mainModuleName)
    runMain(mainModule, Some(projectPath), additionalArgs)
  }

  private def runSingleFile(
    context: PolyglotContext,
    file: File,
    additionalArgs: Array[String]
  ): Unit = {
    val mainModule = context.evalModule(file)
    runMain(mainModule, Some(file), additionalArgs)
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
    additionalArgs: Array[String],
    mainMethodName: String = "main"
  ): Unit = {
    try {
      val mainType = mainModule.getAssociatedType
      val mainFun  = mainModule.getMethod(mainType, mainMethodName)
      mainFun match {
        case Some(main) if mainMethodName != "main" =>
          main.execute(mainType)
        case Some(main) =>
          // Opportunistically parse arguments and convert to ints.
          // This avoids conversions in main function.
          val parsedArgs: Seq[AnyRef] = ArraySeq
            .unsafeWrapArray(additionalArgs)
            .map(arg =>
              try {
                Integer.valueOf(arg)
              } catch {
                case _: NumberFormatException =>
                  arg
              }
            )
          val res = main.execute(parsedArgs: _*)
          if (!res.isNull()) {
            out.println(res);
          }
        case None =>
          err.println(
            s"The module ${mainModule.getName} does not contain a `main` " +
            s"function. It could not be run."
          )
      }
    } catch {
      case e: PolyglotException =>
        printPolyglotException(e, rootPkgPath)
        exitFail()
    }
  }

  /** Handles the `--repl` CLI option
    *
    * @param projectPath if specified, the REPL is run in context of a project
    *                    at the given path
    * @param logLevel    log level to set for the engine runtime
    * @param logMasking  is the log masking enabled
    * @param enableIrCaches are IR caches enabled
    */
  private def runRepl(
    projectPath: Option[String],
    logLevel: LogLevel,
    logMasking: Boolean,
    enableIrCaches: Boolean
  ): Unit = {
    val mainMethodName = "internal_repl_entry_point___"
    val dummySourceToTriggerRepl =
      s"""from Standard.Base import all
         |import Standard.Base.Runtime.Debug
         |
         |$mainMethodName = Debug.breakpoint
         |""".stripMargin
    val replModuleName = "Internal_Repl_Module___"
    val projectRoot    = projectPath.getOrElse("")
    val context =
      new ContextFactory().create(
        projectRoot,
        System.in,
        System.out,
        Repl(TerminalIO()),
        logLevel,
        logMasking,
        enableIrCaches
      )
    val mainModule =
      context.evalModule(dummySourceToTriggerRepl, replModuleName)
    runMain(
      mainModule,
      None,
      new Array[String](0),
      mainMethodName = mainMethodName
    )
    exitSuccess()
  }

  /** Handles `--server` CLI option
    *
    * @param line     a CLI line
    * @param logLevel log level to set for the engine runtime
    */
  private def runLanguageServer(line: CommandLine, logLevel: LogLevel): Unit = {
    val maybeConfig = parseServerOptions(line)

    maybeConfig match {
      case Left(errorMsg) =>
        System.err.println(errorMsg)
        exitFail()

      case Right(config) =>
        LanguageServerApp.run(
          config,
          logLevel,
          line.hasOption(DAEMONIZE_OPTION)
        )
        exitSuccess()
    }
  }

  private def parseServerOptions(
    line: CommandLine
  ): Either[String, LanguageServerConfig] =
    for {
      rootId <- Option(line.getOptionValue(ROOT_ID_OPTION))
        .toRight("Root id must be provided")
        .flatMap { id =>
          Either
            .catchNonFatal(UUID.fromString(id))
            .leftMap(_ => "Root must be UUID")
        }
      rootPath <- Option(line.getOptionValue(ROOT_PATH_OPTION))
        .toRight("Root path must be provided")
      interface = Option(line.getOptionValue(INTERFACE_OPTION))
        .getOrElse("127.0.0.1")
      rpcPortStr = Option(line.getOptionValue(RPC_PORT_OPTION))
        .getOrElse("8080")
      rpcPort <- Either
        .catchNonFatal(rpcPortStr.toInt)
        .leftMap(_ => "Port must be integer")
      dataPortStr = Option(line.getOptionValue(DATA_PORT_OPTION))
        .getOrElse("8081")
      dataPort <- Either
        .catchNonFatal(dataPortStr.toInt)
        .leftMap(_ => "Port must be integer")
      profilingPathStr =
        Option(line.getOptionValue(LANGUAGE_SERVER_PROFILING_PATH))
      profilingPath <- Either
        .catchNonFatal(profilingPathStr.map(Paths.get(_)))
        .leftMap(_ => "Profiling path is invalid")
      profilingTimeStr = Option(
        line.getOptionValue(LANGUAGE_SERVER_PROFILING_TIME)
      )
      profilingTime <- Either
        .catchNonFatal(profilingTimeStr.map(_.toInt.seconds))
        .leftMap(_ => "Profiling time should be an integer")
      profilingEventsLogPathStr =
        Option(line.getOptionValue(LANGUAGE_SERVER_PROFILING_EVENTS_LOG_PATH))
      profilingEventsLogPath <- Either
        .catchNonFatal(profilingEventsLogPathStr.map(Paths.get(_)))
        .leftMap(_ => "Profiling events log path is invalid")
    } yield boot.LanguageServerConfig(
      interface,
      rpcPort,
      dataPort,
      rootId,
      rootPath,
      ProfilingConfig(profilingEventsLogPath, profilingPath, profilingTime)
    )

  /** Prints the version of the Enso executable.
    *
    * @param useJson whether the output should be JSON or human-readable.
    */
  def displayVersion(useJson: Boolean): Unit = {
    val versionDescription = VersionDescription.make(
      "Enso Compiler and Runtime",
      includeRuntimeJVMInfo = true,
      customVersion         = Some(CurrentVersion.version.toString)
    )
    println(versionDescription.asString(useJson))
  }

  /** Parses the log level option.
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

  /** Parses an URI that specifies the logging service connection.
    */
  def parseUri(string: String): Uri =
    try {
      Uri(string)
    } catch {
      case _: IllegalUriException =>
        System.err.println(s"`$string` is not a valid URI.")
        exitFail()
    }

  /** Default log level to use if the LOG_LEVEL option is not provided.
    */
  val defaultLogLevel: LogLevel = LogLevel.Error

  /** Main entry point for the CLI program.
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
    val logMasking = !line.hasOption(NO_LOG_MASKING)
    RunnerLogging.setup(connectionUri, logLevel, logMasking)

    if (line.hasOption(NEW_OPTION)) {
      createNew(
        path           = line.getOptionValue(NEW_OPTION),
        nameOption     = Option(line.getOptionValue(PROJECT_NAME_OPTION)),
        authorName     = Option(line.getOptionValue(PROJECT_AUTHOR_NAME_OPTION)),
        authorEmail    = Option(line.getOptionValue(PROJECT_AUTHOR_EMAIL_OPTION)),
        templateOption = Option(line.getOptionValue(PROJECT_TEMPLATE_OPTION))
      )
    }

    if (line.hasOption(UPLOAD_OPTION)) {
      val projectRoot =
        Option(line.getOptionValue(IN_PROJECT_OPTION))
          .map(Path.of(_))
          .getOrElse {
            logger.error(
              s"When uploading, the $IN_PROJECT_OPTION is mandatory " +
              s"to specify which project to upload."
            )
            exitFail()
          }

      try {
        ProjectUploader.uploadProject(
          projectRoot  = projectRoot,
          uploadUrl    = line.getOptionValue(UPLOAD_OPTION),
          authToken    = Option(line.getOptionValue(AUTH_TOKEN)),
          showProgress = !line.hasOption(HIDE_PROGRESS),
          logLevel     = logLevel
        )
        exitSuccess()
      } catch {
        case UploadFailedError(_) =>
          // We catch this error to avoid printing an unnecessary stack trace.
          // The error itself is already logged.
          exitFail()
      }
    }

    if (line.hasOption(UPDATE_MANIFEST_OPTION)) {
      val projectRoot =
        Option(line.getOptionValue(IN_PROJECT_OPTION))
          .map(Path.of(_))
          .getOrElse {
            logger.error(
              s"The $IN_PROJECT_OPTION is mandatory."
            )
            exitFail()
          }

      try {
        ProjectUploader.updateManifest(projectRoot, logLevel)
      } catch {
        case NonFatal(err) =>
          err.printStackTrace()
          exitFail()
      }
      exitSuccess()
    }

    if (line.hasOption(COMPILE_OPTION)) {
      val packagePaths = line.getOptionValue(COMPILE_OPTION)
      val shouldCompileDependencies =
        !line.hasOption(NO_COMPILE_DEPENDENCIES_OPTION);
      val shouldUseGlobalCache = !line.hasOption(NO_GLOBAL_CACHE_OPTION)

      compile(
        packagePaths,
        shouldCompileDependencies,
        shouldUseGlobalCache,
        logLevel,
        logMasking
      )
    }

    if (line.hasOption(RUN_OPTION)) {
      run(
        line.getOptionValue(RUN_OPTION),
        line.getArgs,
        Option(line.getOptionValue(IN_PROJECT_OPTION)),
        logLevel,
        logMasking,
        shouldEnableIrCaches(line),
        line.hasOption(AUTO_PARALLELISM_OPTION),
        line.hasOption(INSPECT_OPTION),
        line.hasOption(DUMP_GRAPHS_OPTION)
      )
    }
    if (line.hasOption(REPL_OPTION)) {
      runRepl(
        Option(line.getOptionValue(IN_PROJECT_OPTION)),
        logLevel,
        logMasking,
        shouldEnableIrCaches(line)
      )
    }
    if (line.hasOption(DOCS_OPTION)) {
      genDocs(
        Option(line.getOptionValue(IN_PROJECT_OPTION)),
        logLevel,
        logMasking,
        shouldEnableIrCaches(line)
      )
    }
    if (line.hasOption(PREINSTALL_OPTION)) {
      preinstallDependencies(
        Option(line.getOptionValue(IN_PROJECT_OPTION)),
        logLevel
      )
    }
    if (line.hasOption(LANGUAGE_SERVER_OPTION)) {
      runLanguageServer(line, logLevel)
    }
    if (line.getOptions.isEmpty) {
      printHelp(options)
      exitFail()
    }
  }

  /** Checks whether IR caching should be enabled.o
    *
    * The (mutually exclusive) flags can control it explicitly, otherwise it
    * defaults to off in development builds and on in production builds.
    *
    * @param line the command-line
    * @return `true` if caching should be enabled, `false`, otherwise
    */
  private def shouldEnableIrCaches(line: CommandLine): Boolean = {
    if (line.hasOption(IR_CACHES_OPTION)) {
      true
    } else if (line.hasOption(NO_IR_CACHES_OPTION)) {
      false
    } else {
      !isDevBuild
    }
  }
}
