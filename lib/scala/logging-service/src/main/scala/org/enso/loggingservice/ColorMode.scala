package org.enso.loggingservice

/** Describes possible modes of color display in console output. */
sealed trait ColorMode
object ColorMode {

  /** Never use color escape sequences in the output. */
  case object Never extends ColorMode

  /** Enable color output if it seems to be supported. */
  case object Auto extends ColorMode

  /** Always use escape sequences in the output, even if the program thinks they
    * are unsupported.
    *
    * May be useful if output is piped to other programs that know how to handle
    * the escape sequences.
    */
  case object Always extends ColorMode
}
