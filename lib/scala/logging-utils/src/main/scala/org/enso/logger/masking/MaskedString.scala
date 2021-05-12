package org.enso.logger.masking

/** A string that is masked when logged.
  *
  * @param value the underlying string.
  */
case class MaskedString(value: String) extends ToMaskedString {

  /** @inheritdoc */
  override def toString: String =
    value

  /** @inheritdoc */
  override def toMaskedString(shouldMask: Boolean): String =
    if (shouldMask) STUB else value
}
