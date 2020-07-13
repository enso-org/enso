package org.enso.launcher.cli

import cats.{Functor, Semigroupal}

sealed trait Opts[A] {
  // aggregate known names for printing failures
  // aggregate callbacks for setting a value (argument priorities - trailing last)
  // callbacks set values and verify duplicated params (but the possible failure is saved to be reported within result)
  val flags: Seq[(String, Unit => Unit)]
  val parameters: Seq[(String, String => Unit)]
  val prefixedParameters: Seq[(String, String => Unit)]
  // how do we know which to choose? maybe have some mutable counter somwhere ??
  val requiredArguments: Seq[String => Unit]
  val optionalArguments: Seq[String => Unit]
  val trailingArguments: Option[Seq[String] => Unit]
  val additionalArguments: Option[Seq[String] => Unit]

  def gatherParameterNames: Seq[String] =
    parameters.map(_._1) ++ flags.map(_._1)
  def gatherParameterPrefixes: Seq[String] = prefixedParameters.map(_._1)

  // a reset callback to allow multiple calls
  def reset(): Unit

  // result verifies if the value was set and passes it on
  def result(): Either[List[String], A]
}

// TODO [RW] This is a draft in progress, proper documentation will come when the API is stable
object Opts {
  implicit val semigroupal: Semigroupal[Opts] = new Semigroupal[Opts] {
    override def product[A, B](fa: Opts[A], fb: Opts[B]): Opts[(A, B)] = ???
  }

  implicit val functor: Functor[Opts] = new Functor[Opts] {
    override def map[A, B](fa: Opts[A])(f: A => B): Opts[B] = ???
  }

  def positionalArgument[A: Argument](metavar: String = "arg"): Opts[A] = {
    val _: Either[String, A] = Argument[A].read("TODO")
    println(metavar)
    ???
  }

  def optionalArgument[A: Argument](
    metavar: String = "[arg]"
  ): Opts[Option[A]] = {
    val _: Either[String, A] = Argument[A].read("TODO")
    println(metavar)
    ???
  }

  def trailingPositionalArguments[A: Argument](
    metavar: String = "args"
  ): Opts[Seq[A]] = {
    val _: Either[String, A] = Argument[A].read("TODO")
    println(metavar)
    ???
  }

  // `--name`
  def flag(name: String): Opts[Boolean]              = flag(name, None)
  def flag(name: String, short: Char): Opts[Boolean] = flag(name, Some(short))
  private def flag(name: String, short: Option[Char]): Opts[Boolean] = {
    println((name, short))
    ???
  }

  def parameter[A: Argument](name: String): Opts[A] = {
    println(name)
    ???
  }

  def optionalParameter[A: Argument](name: String): Opts[Option[A]] = {
    println(name)
    ???
  }

  // `--prefix.key=value` or `--prefix.key value`
  def prefixedParameters(
    prefix: String
  ): Opts[(String, String)] = {
    println(prefix)
    ???
  }

  // additional parameters, after a `--`
  def additionalParameters: Opts[Seq[String]] = ???

  def pure[A](a: A): Opts[A] = {
    val _ = a
    ???
  }

  def parse[A](opts: Opts[A])(args: Seq[String]): Either[List[String], A] = {
    println(opts)
    println(args)
    ???
  }
}
