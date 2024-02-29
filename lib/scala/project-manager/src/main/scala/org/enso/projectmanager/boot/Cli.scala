package org.enso.projectmanager.boot

import org.apache.commons.cli
import org.enso.polyglot.LanguageInfo

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

  val FILESYSTEM_LIST = "filesystem-list"

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

    val filesystemList: cli.Option = cli.Option.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("path")
      .longOpt(FILESYSTEM_LIST)
      .desc("List directory.")
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
      .addOption(option.filesystemList)

  /** Parse the command line options. */
  def parse(args: Array[String]): Either[String, cli.CommandLine] = {
    Try(new cli.DefaultParser().parse(options, args)).toEither.left
      .map(_.getMessage)
  }

  /** Print the help message to the standard output. */
  def printHelp(): Unit =
    new cli.HelpFormatter().printHelp(LanguageInfo.ID, options)

}
