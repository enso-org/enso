package org.enso.logger

/** Indicates that an object has a custom string representation masking some
  * personally identifiable information.
  */
trait ToMaskedString {

  /** String representation of this object with masked personally identifiable
    * information.
    */
  def toMaskedString: String

  /** A substitution for the masked data. */
  final protected val STUB: String = MaskingUtils.STUB

  /** A synonym for `STUB`. */
  final protected val *** = STUB
}
