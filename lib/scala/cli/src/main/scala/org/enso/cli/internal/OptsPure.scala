package org.enso.cli.internal

class OptsPure[A](v: A) extends BaseOpts[A] {
  override private[cli] def result() = Right(v)
  override private[cli] def reset(): Unit = {}

  override def availableOptionsHelp(): Seq[String] = Seq()
}
