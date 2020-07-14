package org.enso.launcher.cli

import cats.{Functor, Semigroupal}
import org.enso.launcher.cli.impl.{
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
  TrailingArguments
}

case class IllegalOptsStructure(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause)

trait Opts[A] {
  // aggregate known names for printing failures
  // aggregate callbacks for setting a value (argument priorities - trailing last)
  // callbacks set values and verify duplicated params (but the possible failure is saved to be reported within result)
  private[cli] val flags: Map[String, () => Unit]
  private[cli] val parameters: Map[String, String => Unit]
//  private[cli] val requiredParameters: Seq[(String, String)]
  private[cli] val prefixedParameters: Map[String, (String, String) => Unit]

  private[cli] def wantsArgument():              Boolean
  private[cli] def consumeArgument(arg: String): Unit
  private[cli] val requiredArguments: Seq[String]
  private[cli] val optionalArguments: Seq[String]
  private[cli] val trailingArguments: Option[String]

  private[cli] val additionalArguments: Option[Seq[String] => Unit]

  private[cli] def gatherParameterNames: Seq[String] =
    (parameters.keys ++ flags.keys).toSeq
  private[cli] def gatherParameterPrefixes: Seq[String] =
    prefixedParameters.keys.toSeq

  // a reset callback to allow multiple calls
  private[cli] def reset(): Unit

  // result verifies if the value was set and passes it on
  private[cli] def result(): Either[List[String], A]

  def helpExplanations(): Seq[String]
  def commandLine(): String = {
    val options =
      if (parameters.nonEmpty || flags.nonEmpty || prefixedParameters.nonEmpty)
        "[options] "
      else ""
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
    sb.toString()
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
  ): Opts[A] = new PositionalArgument[A](metavar, help)

  def optionalArgument[A: Argument](
    metavar: String,
    help: String = ""
  ): Opts[Option[A]] = new OptionalPositionalArgument[A](metavar, help)

  def trailingPositionalArguments[A: Argument](
    metavar: String,
    help: String = ""
  ): Opts[Seq[A]] = new TrailingArguments[A](metavar, help)

  // `--name`
  def flag(name: String, help: String): Opts[Boolean] =
    new Flag(name, None, help)
  def flag(name: String, short: Char, help: String): Opts[Boolean] =
    new Flag(name, Some(short), help)

  def parameter[A: Argument](
    name: String,
    metavar: String,
    help: String
  ): Opts[A] = new Parameter[A](name, metavar, help)

  def optionalParameter[A: Argument](
    name: String,
    metavar: String,
    help: String
  ): Opts[Option[A]] = new OptionalParameter[A](name, metavar, help)

  // `--prefix.key=value` or `--prefix.key value`
  def prefixedParameters(
    prefix: String,
    help: String = ""
  ): Opts[Seq[(String, String)]] = new PrefixedParameters(prefix, help)

  // additional parameters, after a `--`
  def additionalArguments(help: String = ""): Opts[Seq[String]] =
    new AdditionalArguments(help)

  def pure[A](a: A): Opts[A] = new OptsPure[A](a)

  def parse[A](opts: Opts[A])(args: Seq[String]): Either[List[String], A] = {
    val _ = opts
    val _ = args
    Left(List("deprecated")) // FIXME [RW] remove this
  }
}
