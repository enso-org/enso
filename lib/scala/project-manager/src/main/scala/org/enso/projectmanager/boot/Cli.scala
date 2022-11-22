package org.enso.projectmanager.boot

import org.apache.commons.cli
import org.enso.polyglot.LanguageInfo

import scala.util.Try

object Cli {

  val JSON_OPTION               = "json"
  val HELP_OPTION               = "help"
  val NO_LOG_MASKING            = "no-log-masking"
  val VERBOSE_OPTION            = "verbose"
  val VERSION_OPTION            = "version"
  val PROFILING_PATH            = "profiling-path"
  val PROFILING_TIME            = "profiling-time"
  val PROFILING_EVENTS_LOG_PATH = "profiling-events-log-path"

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

    val profilingEventsLogPath: cli.Option = cli.Option.builder
      .hasArg(true)
      .numberOfArgs(1)
      .argName("file")
      .longOpt(PROFILING_EVENTS_LOG_PATH)
      .desc(
        "The path to the runtime events log file. Enables the runtime events logging."
      )
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
      .addOption(option.profilingEventsLogPath)

  /** Parse the command line options. */
  def parse(args: Array[String]): Either[String, cli.CommandLine] = {
    Try(new cli.DefaultParser().parse(options, args)).toEither.left
      .map(_.getMessage)
  }

  /** Print the help message to the standard output. */
  def printHelp(): Unit =
    new cli.HelpFormatter().printHelp(LanguageInfo.ID, options)

}
