package org.enso.logger.masking

/** A string that is masked when logged.
  *
  * @param value the underlying string.
  */
case class MaskedString(value: String) extends ToLogString {

  /** @inheritdoc */
  override def toString: String =
    value

  /** @inheritdoc */
  override def toLogString(shouldMask: Boolean): String =
    if (shouldMask) STUB else value
}
