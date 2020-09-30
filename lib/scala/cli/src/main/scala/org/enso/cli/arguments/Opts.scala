package org.enso.cli.arguments

import cats.data.NonEmptyList
import cats.implicits._
import cats.{Functor, Semigroupal}
import org.enso.cli.CLIOutput
import org.enso.cli.internal._
import org.enso.cli.internal.opts._

/**
  * Represents a set of options (flags, parameters, arguments) and the logic
  * that converts all of them into a value of type A.
  *
  * Opts instances are allowed to use internal mutable state for parsing. They
  * can be parsed multiple times, but they are not thread-safe.
  */
trait Opts[+A] {

  /**
    * Maps flag names to callbacks that are run for that flag.
    */
  private[cli] def flags: Map[String, () => Unit]

  /**
    * Maps parameter names to callbacks that are run for that parameter.
    */
  private[cli] def parameters: Map[String, String => Unit]

  /**
    * Maps prefixes to callbacks that are run for a prefixed parameter with that
    * prefix.
    */
  private[cli] def prefixedParameters: Map[String, (String, String) => Unit]

  /**
    * Specifies if this set of options can take another argument.
    */
  private[cli] def wantsArgument(): Boolean

  /**
    * A callback for arguments.
    *
    * Should not be called if [[wantsArgument]] returns false.
    *
    * @param arg argument to consume
    * @param commandPrefix current command prefix to display in error/help
    *                      messages
    * @param suppressUnexpectedArgument if set, unexpected argument error should
    *                                   be suppressed
    */
  private[cli] def consumeArgument(
    arg: String,
    commandPrefix: Seq[String],
    suppressUnexpectedArgument: Boolean
  ): ParserContinuation

  /**
    * An optional callback for additional arguments. If it is provided, all
    * arguments following `--` are passed to it.
    */
  private[cli] def additionalArguments: Option[Seq[String] => Unit]

  /**
    * A reset callback that should reset all internal state. Allows to reuse an
    * Opts instance several times.
    */
  private[cli] def reset(): Unit

  /**
    * Called at the end of parsing to obtain the result.
    *
    * Any errors accumulated throughout parsing should be returned here. This is
    * the right moment to do final validation (for example, detecting missing
    * options).
    */
  private[cli] def result(commandPrefix: Seq[String]): Either[OptsParseError, A]

  /**
    * Lists options that should be printed in the usage [[commandLines]].
    */
  private[cli] def usageOptions: Seq[String]

  /**
    * Names of required arguments, for printing in the usage [[commandLines]].
    */
  private[cli] def requiredArguments: Seq[String]

  /**
    * Names of optional arguments, for printing in the usage [[commandLines]].
    */
  private[cli] def optionalArguments: Seq[String]

  /**
    * Name of a trailing arguments set, for printing in the usage
    * [[commandLines]].
    */
  private[cli] def trailingArguments: Option[String]

  /**
    * Lists all available options by their name and format.
    *
    * Used for displaying suggestions when an unknown option is provided. Can
    * contain, for example `("setting", "--setting VALUE")`.
    */
  private[cli] def gatherOptions: Seq[(String, String)]

  /**
    * Lists all available prefixed parameters by their name and format.
    *
    * Used for displaying suggestions when an unknown prefix is provided. Can
    * contain, for example `("jvm", "--jvm.KEY VALUE")`.
    */
  private[cli] def gatherPrefixedParameters: Seq[(String, String)]

  /**
    * Lists all available options with their help messages to be displayed in
    * the help.
    */
  def availableOptionsHelp(): Seq[String]

  /**
    * Lists all available prefixed parameters with their help messages to be
    * displayed in the help.
    */
  def availablePrefixedParametersHelp(): Seq[String]

  /**
    * Lists additional help messages that are displayed at the end of the help.
    */
  def additionalHelp(): Seq[String]

  /**
    * A helper function that displays an options command line based on the
    * definition of [[usageOptions]].
    *
    * It does not include any arguments, but only parameters and flags.
    *
    * @return a string representing the options usage
    */
  private[cli] def commandLineOptions(
    alwaysIncludeOtherOptions: Boolean
  ): String = {
    val flagsWithoutDuplicates = flags.keys.count(_.length > 1)

    val allOptions =
      parameters.size + flagsWithoutDuplicates + prefixedParameters.size
    val otherOptions =
      if (alwaysIncludeOtherOptions || allOptions > usageOptions.size)
        " [options]"
      else ""
    otherOptions + usageOptions.map(" " + _).mkString
  }

  /**
    * A helper function that gathers all options and arguments definitions, to
    * display a command line for showing in the usage section of the help.
    *
    * @return a non-empty list of available usages of this option set. Multiple
    *         entries may be returned in presence of subcommands.
    */
  def commandLines(
    alwaysIncludeOtherOptions: Boolean = false
  ): NonEmptyList[String] = {
    val options  = commandLineOptions(alwaysIncludeOtherOptions)
    val required = requiredArguments.map(arg => s" $arg").mkString
    val optional = optionalArguments.map(arg => s" [$arg]").mkString
    val trailing = trailingArguments.map(args => s" [$args...]").getOrElse("")
    val additional =
      if (additionalArguments.isDefined) " [-- <additional arguments>...]"
      else ""
    val sb = new StringBuilder
    sb.append(options)
    sb.append(required)
    sb.append(optional)
    sb.append(trailing)
    sb.append(additional)
    NonEmptyList.one(sb.toString().stripLeading())
  }

  /**
    * Generates explanations of parameters to be included in the help message.
    */
  def helpExplanations(): String = {
    val options =
      availableOptionsHelp().map(CLIOutput.indent + _).mkString("\n")
    val optionsHelp =
      if (options.isEmpty) "" else "\nAvailable options:\n" + options + "\n"

    val prefixed =
      availablePrefixedParametersHelp().map(CLIOutput.indent + _).mkString("\n")
    val prefixedHelp =
      if (prefixed.isEmpty) ""
      else
        "\nPrefixed parameters (may occur multiple times):\n" + prefixed + "\n"

    val additional     = additionalHelp().map(_ + "\n").mkString
    val additionalText = if (additional.isEmpty) "" else "\n" + additional
    optionsHelp + prefixedHelp + additionalText
  }

  /**
    * Generates a help text for the command, including usage, available options
    * and any additional help lines.
    *
    * @param commandPrefix list of command names that should prefix the
    *                      commandline in usage. The first entry in that list
    *                      should be the application name and the following
    *                      entries are commands/subcommands.
    */
  def help(commandPrefix: Seq[String]): String = {
    val prefix    = commandPrefix.mkString(" ")
    val usages    = commandLines().map(s"$prefix\t" + _)
    val firstLine = "Usage: "
    val padding   = " " * firstLine.length
    val usage =
      firstLine + usages.head +
      usages.tail.map("\n" + padding + _).mkString + "\n"

    usage + helpExplanations().stripTrailing()
  }

  /**
    * Renders text explaining how to display the help.
    */
  def shortHelp(commandPrefix: Seq[String]): String =
    s"See `${commandPrefix.mkString(" ")} --help` for usage explanation."
}

/**
  * The external Opts API is inspired by https://github.com/bkirwi/decline but
  * it is simplified in some places and extended in others to allow for the
  * features that we need. The internal implementation differs a lot, as the
  * main priority of its design was providing the best possible error messages.
  *
  * Opts implement the [[Semigroupal]] and [[Functor]] instances, which allows
  * to use them with the `mapN` function. The idea is that a set of options may
  * be combined and mapped into an overall result. For example:
  * {{{
  * val flag: Opts[Boolean] = Opts.flag("flag", "Help text.", false)
  * val arg: Opts[Int] = Opts.positionalArgument[Int]("ARG")
  * val combined: Opts[String] = (flag, arg) mapN { (flagIsSet, argValue) =>
  *   s"Flag was $flagIsSet and the argument was $argValue"
  *  }
  * }}}
  * Parsing the resulting `combined` value will yield a string that summarizes
  * the values of the two options.
  *
  * It is very important to note that while the order of flags and parameters
  * does not matter, the order in which arguments are combined does matter. The
  * arguments are parsed in the order they appear in the combined tuple.
  * Required arguments should always go first, followed by any optional
  * arguments. Trailing arguments should go at the end. If an optional argument
  * were placed before a required one, it would not be possible to provide the
  * required argument without providing the optional one, so it would
  * effectively become required too. As trailing arguments accept arbitrarily
  * many arguments, any arguments placed after them will never be handled, as
  * all arguments will be captured by the trailing arguments.
  */
object Opts {
  object implicits {

    /**
      * A [[Semigroupal]] instance for combining multiple options sets.
      *
      * The combined set of options parses options from both sets and returns a
      * tuple of results of both sets if both have succeeded. Unordered options
      * (i.e. flags and parameters) can be interleaved. Arguments are parsed in
      * order, i.e. first all arguments of the first set are parsed and only
      * after the first set does not want any more arguments, the second set is
      * given any further arguments. If the first set contained an option
      * accepting an unlimited amount of arguments (like the
      * [[trailingPositionalArguments]]), arguments in the second set, if
      * present, will never be handled. So it is important to keep
      * [[trailingPositionalArguments]] last.
      */
    implicit val semigroupal: Semigroupal[Opts] = new Semigroupal[Opts] {
      override def product[A, B](fa: Opts[A], fb: Opts[B]): Opts[(A, B)] =
        new OptsProduct(fa, fb)
    }

    /**
      * A [[Functor]] instance that allows to map over option sets to process
      * their values.
      *
      * The mapping is done eagerly, and it is possible that the result of
      * mapping some set of options that is later combined into a bigger one,
      * may still later fail. Thus the function doing the mapping should avoid
      * any  side-effects. It is recommended to return an IO monad or a
      * `() => A` callback to handle actions.
      */
    implicit val functor: Functor[Opts] = new Functor[Opts] {
      override def map[A, B](fa: Opts[A])(f: A => B): Opts[B] =
        new OptsMap(fa, f)
    }

    implicit class WithDefaultSyntax[A](val opts: Opts[Option[A]]) {

      /**
        * Adds a default value that is returned if the result was None, turning
        * `Opts[Option[A]]` into `Opts[A]`.
        */
      def withDefault(defaultValue: => A): Opts[A] =
        opts.map(_.getOrElse(defaultValue))
    }

    implicit class HiddenSyntax[A](val opts: Opts[A]) {

      /**
        * Makes options from this Opts instance hidden in any help messages. Can
        * be used for internal configuration.
        */
      def hidden: Opts[A] = new HiddenOpts(opts)
    }

    implicit class MapWithErrorsSyntax[A](val opts: Opts[A]) {

      /**
        * Allows to map an Opts instance in a way that may result in an error.
        *
        * If `f` returns a [[Left]], a parse error is reported. Otherwise,
        * proceeds as `map` would with the result of `Right`.
        */
      def mapWithErrors[B](f: A => Either[OptsParseError, B]): Opts[B] =
        new OptsMapWithErrors(opts, f)
    }
  }

  import implicits._

  /**
    * An option that accepts a single (required) positional argument and returns
    * its value.
    *
    * Fails if an argument is not provided.
    *
    * @param metavar the name of the argument used in help
    * @param help an additional help message that can be displayed, explaining
    *             what is this argument
    * @tparam A type of the argument that is parsed; needs an [[Argument]]
    *           instance
    */
  def positionalArgument[A: Argument](
    metavar: String,
    help: String = ""
  ): Opts[A] =
    new PositionalArgument[A](metavar, if (help.isEmpty) None else Some(help))

  /**
    * An option that accepts a single optional positional argument and returns
    * an [[Option]] containing its value, if it was provided.
    *
    * @param metavar the name of the argument used in help
    * @param help an additional help message that can be displayed, explaining
    *             what is this argument
    * @tparam A type of the argument that is parsed; needs an [[Argument]]
    *           instance
    */
  def optionalArgument[A: Argument](
    metavar: String,
    help: String = ""
  ): Opts[Option[A]] =
    new OptionalPositionalArgument[A](
      metavar,
      if (help.isEmpty) None else Some(help)
    )

  /**
    * An option that accepts an arbitrary amount of arguments and returns a
    * (possibly empty) list of parsed arguments.
    *
    * @param metavar the name of the argument used in help
    * @param help an additional help message that can be displayed, explaining
    *             what are these arguments
    * @tparam A type of the argument that is parsed; needs an [[Argument]]
    *           instance
    */
  def trailingPositionalArguments[A: Argument](
    metavar: String,
    help: String = ""
  ): Opts[Seq[A]] =
    new TrailingArguments[A](metavar, if (help.isEmpty) None else Some(help))

  /**
    * A flag that returns a [[Boolean]] value indicating if it was provided in
    * the command line.
    *
    * Returns true if `--name` is included in the command line.
    *
    * @param name the name of the flag
    * @param help the help message included in the available options list
    * @param showInUsage specifies whether this flag should be included in the
    *                    usage command line
    */
  def flag(name: String, help: String, showInUsage: Boolean): Opts[Boolean] =
    new Flag(name, None, help, showInUsage)

  /**
    * A flag with a short alternative that returns a [[Boolean]] value
    * indicating if it was provided in the command line.
    *
    * Returns true if `--name` or `-s` (where `s` is the short argument) is
    * included in the command line.
    *
    * This version should not be used most of the time, as we prefer long flag
    * names.
    *
    * @param name the name of the flag
    * @param short the one letter alternative that sets this flag
    * @param help the help message included in the available options list
    * @param showInUsage specifies whether this flag should be included in the
    *                    usage command line
    */
  def flag(
    name: String,
    short: Char,
    help: String,
    showInUsage: Boolean
  ): Opts[Boolean] =
    new Flag(name, Some(short), help, showInUsage)

  /**
    * A (required) parameter that tries to parse its value as type A.
    *
    * If a required parameter is not provided, an error is reported.
    *
    * Supports both `--name VALUE` and `--name=VALUE` syntax.
    *
    * @param name name of the parameter
    * @param metavar the name of the argument of the parameter, displayed in
    *                help
    * @param help the help message included in the available options list
    * @tparam A type of the value that is parsed; needs an [[Argument]]
    *           instance
    */
  def parameter[A: Argument](
    name: String,
    metavar: String,
    help: String
  ): Opts[A] = new Parameter[A](name, metavar, help)

  /**
    * An optional parameter that tries to parse its value as type A. It returns
    * an [[Option]] that is defined if the parameter was provided.
    *
    * @param name name of the parameter
    * @param metavar the name of the argument of the parameter, displayed in
    *                help
    * @param help the help message included in the available options list
    * @param showInUsage specifies whether this flag should be included in the
    *                    usage command line
    * @tparam A type of the value that is parsed; needs an [[Argument]]
    *           instance
    */
  def optionalParameter[A: Argument](
    name: String,
    metavar: String,
    help: String,
    showInUsage: Boolean = false
  ): Opts[Option[A]] =
    new OptionalParameter[A](name, metavar, help, showInUsage)

  /**
    * An optional parameter with multiple aliases.
    *
    * Returns a value if it is present for exactly one of the aliases or none if
    * no alias is present. If values are present for mulitple aliases, raises a
    * parse error.
    *
    * @param primaryName primary name that is displayed in help and suggestions
    * @param aliases additional aliases
    * @param metavar the name of the argument of the parameter, displayed in
    *                help
    * @param help the help message included in the available options list
    * @param showInUsage specifies whether this flag should be included in the
    *                    usage command line
    * @tparam A type of the value that is parsed; needs an [[Argument]]
    *           instance
    */
  def aliasedOptionalParameter[A: Argument](
    primaryName: String,
    aliases: String*
  )(
    metavar: String,
    help: String,
    showInUsage: Boolean = false
  ): Opts[Option[A]] = {
    def withName(name: String)(option: Option[A]): Option[(A, String)] =
      option.map((_, name))
    val primaryOpt = optionalParameter[A](
      primaryName,
      metavar,
      help,
      showInUsage = showInUsage
    ).map(withName(primaryName))
    val aliasedOpts = aliases.map(aliasName =>
      optionalParameter[A](aliasName, metavar, help, showInUsage = false)
        .map(withName(aliasName))
        .hidden
    )
    sequence(primaryOpt :: aliasedOpts.toList).mapWithErrors { resultOptions =>
      val results = resultOptions.flatten
      results match {
        case Nil             => Right(None)
        case (one, _) :: Nil => Right(Some(one))
        case more =>
          val presentOptions = more.map(res => s"`--${res._2}`")
          OptsParseError.left(
            s"Multiple values for aliases of the same option " +
            s"(${presentOptions.init.mkString(", ")} and " +
            s"${presentOptions.last}) have been provided. Please provide " +
            s"just one value for `--$primaryName`."
          )
      }
    }
  }

  /**
    * An option that accepts an arbitrary amount of parameters with a fixed
    * prefix and returns a sequence of tuples that contain a suffix and a
    * (string) value.
    *
    * Supports both `--prefix.suffix VALUE` and `--prefix.suffix=VALUE` syntax.
    * The prefix and suffix have to be separated by a dot. The suffix can
    * contain more dots.
    *
    * @param prefix the prefix that the parameter must contain to be parsed by
    *               this instance.
    * @param help an additional help message that can be displayed, explaining
    *             what is this set of parameters
    */
  def prefixedParameters(
    prefix: String,
    help: String         = "",
    keyMetavar: String   = "KEY",
    valueMetavar: String = "VALUE"
  ): Opts[Seq[(String, String)]] =
    new PrefixedParameters(
      prefix,
      keyMetavar,
      valueMetavar,
      if (help.isEmpty) None else Some(help)
    )

  /**
    * An option that accepts an arbitrary amount of unspecified additional
    * arguments and options (flags or parameters included). Any values following
    * a `--` are passed this option.
    *
    * It can be used to accept possibly unknown parameters that are passed to
    * other applications.
    *
    * @param help an additional help message
    */
  def additionalArguments(help: String = ""): Opts[Seq[String]] =
    new AdditionalArguments(help)

  /**
    * An option that allows to create subcommands. It accepts a single argument
    * which should be the name of the command that is chosen. Any parameters
    * following that argument are parsed according to the logic defined for the
    * selected command.
    *
    * When combined with other options, any parameters passed after the
    * subcommand name can be parsed either by the subcommand logic or the logic
    * of other options that it is combined with.
    *
    * @param firstCommand first subcommand
    * @param otherCommands any following subcommands
    */
  def subcommands[A](
    firstCommand: Command[A],
    otherCommands: Command[A]*
  ): Opts[A] = {
    val nonEmptyCommands = NonEmptyList.of(firstCommand, otherCommands: _*)
    new SubcommandOpt[A](nonEmptyCommands)
  }

  /**
    * A helper option that does no parsing and simply returns the provided
    * value.
    */
  def pure[A](a: A): Opts[A] = new OptsPure[A](a)

  /**
    * Turns a sequence of options into a single option that returns results of
    * these options, if all of them parsed successfully.
    */
  def sequence[A](opts: Seq[Opts[A]]): Opts[Seq[A]] =
    sequence(opts.toList).map(_.toSeq)

  /**
    * Turns a list of options into a single option that returns results of
    * these options, if all of them parsed successfully.
    */
  def sequence[A](opts: List[Opts[A]]): Opts[List[A]] =
    opts match {
      case Nil => Opts.pure(Nil)
      case head :: tail =>
        val tailSequenced = sequence(tail)
        (head, tailSequenced) mapN { (headResult, tailResult) =>
          headResult :: tailResult
        }
    }
}
