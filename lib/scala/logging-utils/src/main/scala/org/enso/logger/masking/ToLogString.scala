package org.enso.logger.masking

/** Indicates that an object has a custom string representation masking some
  * personally identifiable information.
  *
  * == Logging ==
  * This object should be supplied to the logger as an argument of the template
  * string. This way the logger will use the `toLogString` representation of
  * the object.
  *
  * {{{
  *   log.debug("Created [{}].", obj)
  * }}}
  *
  * == Errors ==
  * Note that the string interpolation `s"Created [$obj]"` still uses the
  * default `toString` implementation. When creating errors, you should use
  * the `applyMasking()` function that returns the masked representation
  * depending on whether or not the masking is enabled in the application.
  *
  * {{{
  *   throw new Exception(s"Failed to initialize [${obj.applyMasking}].")
  * }}}
  */
trait ToLogString {

  /** A substitution for the masked data. */
  final protected val STUB: String = MaskingUtils.STUB

  /** A synonym for the `STUB`. */
  final protected val *** = STUB

  /** String representation of this object with masked personally identifiable
    * information.
    *
    * @param shouldMask decides whether or not the value should be masked
    */
  def toLogString(shouldMask: Boolean): String

  /** Returns the string representation defined by the `toMaskedString` method,
    * based on the current masking settings.
    */
  def applyMasking(): String =
    Masking().mask(this).toString

}
