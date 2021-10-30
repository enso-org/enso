package org.enso.cli.internal

/** Specifies parser behaviour after parsing a plain token.
  */
sealed trait ParserContinuation

object ParserContinuation {

  /** Specifies to continue parsing.
    *
    * This variant is returned most of the time in the usual parsing flow.
    */
  case object ContinueNormally extends ParserContinuation

  /** Specifies to stop parsing and return partial results.
    *
    * This can be used to abstain from handling further errors. For example, if
    * a command name is misspelled, any further parameters would be reported as
    * unknown even though they are defined for the right command. To avoid such
    * misleading situations, we stop parsing.
    */
  case object Stop extends ParserContinuation

  /** Specifies that the parser should stop parsing immediately, gather the
    * partial results and finish by returning a closure that will call the
    * provided `continuation`.
    *
    * The continuation is given the sequence of remaining tokens and additional
    * arguments and never returns.
    *
    * This special case is used to implement plugins - when a plugin invocation
    * is detected, all further tokens should be handled by the plugin that will
    * be invoked.
    */
  case class Escape(
    continuation: (Seq[Token], Seq[String]) => Int
  ) extends ParserContinuation
}
