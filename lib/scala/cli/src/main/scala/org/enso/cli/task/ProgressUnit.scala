package org.enso.cli.task

/** Defines the unit in which the progress amount is measured. */
sealed trait ProgressUnit

object ProgressUnit {

  /** Specifies that progress amount is measured in bytes. */
  case object Bytes extends ProgressUnit

  /** Does not specify a particular progress unit. */
  case object Unspecified extends ProgressUnit
}
