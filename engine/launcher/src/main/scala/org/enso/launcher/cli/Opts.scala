package org.enso.launcher.cli

import cats.data.NonEmptyList
import cats.{Functor, Semigroupal}
import org.enso.launcher.cli.internal.{
  AdditionalArguments,
  Flag,
  OptionalParameter,
  OptionalPositionalArgument,
  OptsMap,
  OptsProduct,
  OptsPure,
  Parameter,
  PositionalArgument,
  PrefixedParameters,
  SubcommandOpt,
  TrailingArguments
}

case class IllegalOptsStructure(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause)

trait Opts[A] {
  // aggregate known names for printing failures
  // aggregate callbacks for setting a value (argument priorities - trailing last)
  // callbacks set values and verify duplicated params (but the possible failure is saved to be reported within result)
  private[cli] def flags:      Map[String, () => Unit]
  private[cli] def parameters: Map[String, String => Unit]
//  private[cli] val requiredParameters: Seq[(String, String)]
  private[cli] def prefixedParameters: Map[String, (String, String) => Unit]

  // options that are explicitly printed in usage
  private[cli] def usageOptions: Seq[String]

  private[cli] def wantsArgument():              Boolean
  private[cli] def consumeArgument(arg: String): Unit
  private[cli] def requiredArguments:            Seq[String]
  private[cli] def optionalArguments:            Seq[String]
  private[cli] def trailingArguments:            Option[String]

  private[cli] def additionalArguments: Option[Seq[String] => Unit]

  private[cli] def gatherOptions: Seq[(String, String)]
  private[cli] def gatherParameterPrefixes: Seq[String] =
    prefixedParameters.keys.toSeq

  // a reset callback to allow multiple calls
  private[cli] def reset(): Unit

  // result verifies if the value was set and passes it on
  private[cli] def result(): Either[List[String], A]

  def helpExplanations(): Seq[String]
  def additionalHelp():   Seq[String]

  def commandLineOptions(): String = {
    val allOptions = parameters.size + flags.size + prefixedParameters.size
    val otherOptions =
      if (allOptions > usageOptions.size)
        "[options] "
      else ""
    otherOptions + usageOptions.map(_ + " ").mkString
  }

  def commandLines(): NonEmptyList[String] = {
    val options  = commandLineOptions()
    val required = requiredArguments.map(arg => s"$arg ").mkString
    val optional = optionalArguments.map(arg => s"[$arg]").mkString
    val trailing = trailingArguments.map(args => s"[$args...]").getOrElse("")
    val additional =
      if (additionalArguments.isDefined) "[-- <additional arguments>...]"
      else ""
    val sb = new StringBuilder
    sb.append(options)
    sb.append(required)
    sb.append(optional)
    sb.append(trailing)
    sb.append(additional)
    NonEmptyList.one(sb.toString())
  }
}

// TODO [RW] This is a draft in progress, proper documentation will come when the API is stable
/**
  * The external Opts API is inspired by https://github.com/bkirwi/decline but
  * it is simplified in some places and extended in others to allow for the
  * features that we need. The internal implementation differs a lot, as our
  * priority was to have as good error messages as possible.
  */
object Opts {
  implicit val semigroupal: Semigroupal[Opts] = new Semigroupal[Opts] {
    override def product[A, B](fa: Opts[A], fb: Opts[B]): Opts[(A, B)] =
      new OptsProduct(fa, fb)
  }

  implicit val functor: Functor[Opts] = new Functor[Opts] {
    override def map[A, B](fa: Opts[A])(f: A => B): Opts[B] = new OptsMap(fa, f)
  }

  def positionalArgument[A: Argument](
    metavar: String,
    help: String = ""
  ): Opts[A] =
    new PositionalArgument[A](metavar, if (help.isEmpty) None else Some(help))

  def optionalArgument[A: Argument](
    metavar: String,
    help: String = ""
  ): Opts[Option[A]] =
    new OptionalPositionalArgument[A](
      metavar,
      if (help.isEmpty) None else Some(help)
    )

  def trailingPositionalArguments[A: Argument](
    metavar: String,
    help: String = ""
  ): Opts[Seq[A]] =
    new TrailingArguments[A](metavar, if (help.isEmpty) None else Some(help))

  // `--name`
  def flag(name: String, help: String, showInUsage: Boolean): Opts[Boolean] =
    new Flag(name, None, help, showInUsage)
  def flag(
    name: String,
    short: Char,
    help: String,
    showInUsage: Boolean
  ): Opts[Boolean] =
    new Flag(name, Some(short), help, showInUsage)

  def parameter[A: Argument](
    name: String,
    metavar: String,
    help: String
  ): Opts[A] = new Parameter[A](name, metavar, help)

  def optionalParameter[A: Argument](
    name: String,
    metavar: String,
    help: String,
    showInUsage: Boolean = false
  ): Opts[Option[A]] =
    new OptionalParameter[A](name, metavar, help, showInUsage)

  // `--prefix.key=value` or `--prefix.key value`
  def prefixedParameters(
    prefix: String,
    help: String = ""
  ): Opts[Seq[(String, String)]] =
    new PrefixedParameters(prefix, if (help.isEmpty) None else Some(help))

  // additional parameters, after a `--`
  def additionalArguments(help: String = ""): Opts[Seq[String]] =
    new AdditionalArguments(help)

  def subcommands[A](commands: Command[A]*): Opts[A] = {
    val nonEmptyCommands = NonEmptyList.fromList(commands.toList).getOrElse {
      throw new IllegalArgumentException(
        "The subcommands option should take at least one command."
      )
    }
    new SubcommandOpt[A](nonEmptyCommands)
  }

  def pure[A](a: A): Opts[A] = new OptsPure[A](a)
}
