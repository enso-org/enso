package org.enso.cli.internal

/** A token used in the parser.
  */
sealed trait Token {

  /** The original value that this token has been created from.
    *
    * Used to reverse the tokenization process.
    */
  def originalValue: String
}

/** Plain token, usually treated as an argument or value for a parameter that is
  * preceding it.
  */
case class PlainToken(override val originalValue: String) extends Token

/** A token representing a parameter or a flag.
  *
  * It has form `--abc`.
  */
case class ParameterOrFlag(parameter: String)(
  override val originalValue: String
) extends Token

/** A token representing a mistyped parameter, i.e. `-abc`.
  *
  * Used for better error handling.
  */
case class MistypedParameter(parameter: String)(
  override val originalValue: String
) extends Token

/** A token representing a parameter with a value, i.e. `--key=value`.
  */
case class ParameterWithValue(parameter: String, value: String)(
  override val originalValue: String
) extends Token
