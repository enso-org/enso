package org.enso.cli.internal.opts

import org.enso.cli.arguments.OptsParseError

class OptsPure[A](v: A) extends BaseOpts[A] {
  override private[cli] def result(
    commandPrefix: Seq[String]
  ): Either[OptsParseError, A] = Right(v)
  override private[cli] def reset(): Unit = {}

  override def availableOptionsHelp(): Seq[String] = Seq()
}
