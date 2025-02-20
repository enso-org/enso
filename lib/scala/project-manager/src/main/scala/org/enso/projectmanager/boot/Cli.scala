package org.enso.projectmanager.boot

import org.apache.commons.cli
import org.enso.common.LanguageInfo

import scala.util.Try

object Cli {

  val JSON_OPTION        = "json"
  val HELP_OPTION        = "help"
  val NO_LOG_MASKING     = "no-log-masking"
  val VERBOSE_OPTION     = "verbose"
  val VERSION_OPTION     = "version"
  val PROFILING_PATH     = "profiling-path"
  val PROFILING_TIME     = "profiling-time"
  val PROJECTS_DIRECTORY = "projects-directory"
  val PROJECT_LIST       = "project-list"

  val FILESYSTEM_EXISTS           = "filesystem-exists"
  val FILESYSTEM_LIST             = "filesystem-list"
  val FILESYSTEM_CREATE_DIRECTORY = "filesystem-create-directory"
  val FILESYSTEM_DELETE           = "filesystem-delete"
  val FILESYSTEM_MOVE_FROM        = "filesystem-move-from"
  val FILESYSTEM_MOVE_TO          = "filesystem-move-to"
  val FILESYSTEM_READ_PATH        = "filesystem-read-path"
  val FILESYSTEM_WRITE_PATH       = "filesystem-write-path"

  object option {

    val help: cli.Option = cli.Option
      .builder("h")
      .longOpt(HELP_OPTION)
      .desc("Displays this message.")
      .build()

    val verbose: cli.Option = cli.Option
      .builder("v")
      .longOpt(VERBOSE_OPTION)
      .desc("Increase logs verbosity. Can be added multiple times (-vv).")
      .build()

    val version: cli.Option = cli.Option.builder
      .longOpt(VERSION_OPTION)
      .desc("Checks the version of the Enso executable.")
      .build()

    val json: cli.Option = cli.Option.builder
      .longOpt(JSON_OPTION)
      .desc("Switches the --version option to JSON output.")
      .build()

    val noLogMasking: cli.Option = cli.Option.builder
      .longOpt(NO_LOG_MASKING)
      .desc(
        "Disable masking of personally identifiable information in logs. " +
        "Masking can be also disabled with the `NO_LOG_MASKING` environment " +
        "variable."
      )
      .build()

    val profilingPath: cli.Option = cli.Option.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("file")
      .longOpt(PROFILING_PATH)
      .desc("The path to profiling file. Enables the application profiling.")
      .build()

    val profilingTime: cli.Option = cli.Option.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("seconds")
      .longOpt(PROFILING_TIME)
      .desc("The duration in seconds limiting the application profiling time.")
      .build()

    val projectsDirectory: cli.Option = cli.Option.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("path")
      .longOpt(PROJECTS_DIRECTORY)
      .desc("The path to the projects directory.")
      .build()

    val projectList: cli.Option = cli.Option.builder
      .optionalArg(true)
      .numberOfArgs(1)
      .`type`(classOf[java.lang.Number])
      .argName("limit")
      .longOpt(PROJECT_LIST)
      .desc("List user projects.")
      .build()

    val filesystemExists: cli.Option = cli.Option.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("path")
      .longOpt(FILESYSTEM_EXISTS)
      .desc("Check if a file or directory exists.")
      .build()

    val filesystemList: cli.Option = cli.Option.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("path")
      .longOpt(FILESYSTEM_LIST)
      .desc("List directory.")
      .build()

    val filesystemCreateDirectory: cli.Option = cli.Option.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("path")
      .longOpt(FILESYSTEM_CREATE_DIRECTORY)
      .desc("Create directory.")
      .build()

    val filesystemDelete: cli.Option = cli.Option.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("path")
      .longOpt(FILESYSTEM_DELETE)
      .desc("Delete file or directory recursively.")
      .build()

    val filesystemMoveFrom: cli.Option = cli.Option.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("path")
      .longOpt(FILESYSTEM_MOVE_FROM)
      .desc("Move directory. Target.")
      .build()

    val filesystemMoveTo: cli.Option = cli.Option.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("path")
      .longOpt(FILESYSTEM_MOVE_TO)
      .desc("Move directory. Destination.")
      .build()

    val filesystemReadPath: cli.Option = cli.Option.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("path")
      .longOpt(FILESYSTEM_READ_PATH)
      .desc("Read the contents of the provided file")
      .build()

    val filesystemWritePath: cli.Option = cli.Option.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("path")
      .longOpt(FILESYSTEM_WRITE_PATH)
      .desc("Write data from stdin to the provided file")
      .build()
  }

  val options: cli.Options =
    new cli.Options()
      .addOption(option.help)
      .addOption(option.verbose)
      .addOption(option.version)
      .addOption(option.json)
      .addOption(option.noLogMasking)
      .addOption(option.profilingPath)
      .addOption(option.profilingTime)
      .addOption(option.projectsDirectory)
      .addOption(option.projectList)
      .addOption(option.filesystemExists)
      .addOption(option.filesystemList)
      .addOption(option.filesystemCreateDirectory)
      .addOption(option.filesystemDelete)
      .addOption(option.filesystemMoveFrom)
      .addOption(option.filesystemMoveTo)
      .addOption(option.filesystemReadPath)
      .addOption(option.filesystemWritePath)

  /** Parse the command line options. */
  def parse(args: Array[String]): Either[String, cli.CommandLine] = {
    Try(new cli.DefaultParser().parse(options, args)).toEither.left
      .map(_.getMessage)
  }

  /** Print the help message to the standard output. */
  def printHelp(): Unit =
    new cli.HelpFormatter().printHelp(LanguageInfo.ID, options)

}
