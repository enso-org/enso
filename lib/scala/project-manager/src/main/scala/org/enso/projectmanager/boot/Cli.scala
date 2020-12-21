package org.enso.projectmanager.boot

import org.apache.commons.cli
import org.enso.polyglot.LanguageInfo

import scala.util.Try

object Cli {

  val JSON_OPTION             = "json"
  val HELP_OPTION             = "help"
  val VERBOSE_OPTION          = "verbose"
  val VERSION_OPTION          = "version"
  val LOCAL_ENGINE_REPOSITORY = "local-engine-repository"
  val LOCAL_GRAAL_REPOSITORY  = "local-graal-repository"

  object option {

    val help: cli.Option = cli.Option
      .builder("h")
      .longOpt(HELP_OPTION)
      .desc("Displays this message.")
      .build()

    val verbose: cli.Option = cli.Option
      .builder("v")
      .longOpt(VERBOSE_OPTION)
      .desc("Increase logs verbosity. Can be added multiple times (-vv)")
      .build()

    val version: cli.Option = cli.Option.builder
      .longOpt(VERSION_OPTION)
      .desc("Checks the version of the Enso executable.")
      .build()

    val json: cli.Option = cli.Option.builder
      .longOpt(JSON_OPTION)
      .desc("Switches the --version option to JSON output.")
      .build()

    val localEngineRepository: cli.Option = cli.Option.builder
      .longOpt(LOCAL_ENGINE_REPOSITORY)
      .hasArg
      .numberOfArgs(1)
      .argName("path")
      .desc(
        "Allows the Project Manager to install engine versions from an " +
        "offline repository."
      )
      .build()

    val localGraalRepository: cli.Option = cli.Option.builder
      .longOpt(LOCAL_GRAAL_REPOSITORY)
      .hasArg
      .numberOfArgs(1)
      .argName("path")
      .desc(
        "Allows the Project Manager to install GraalVM versions from an " +
        "offline repository."
      )
      .build()
  }

  val options: cli.Options =
    new cli.Options()
      .addOption(option.help)
      .addOption(option.verbose)
      .addOption(option.version)
      .addOption(option.json)
      .addOption(option.localEngineRepository)
      .addOption(option.localGraalRepository)

  /** Parse the command line options. */
  def parse(args: Array[String]): Either[String, cli.CommandLine] = {
    Try(new cli.DefaultParser().parse(options, args)).toEither.left
      .map(_.getMessage)
  }

  /** Print the help message to the standard output. */
  def printHelp(): Unit =
    new cli.HelpFormatter().printHelp(LanguageInfo.ID, options)

}
