package org.enso.logger.masking

import java.nio.file.Path

/** A path that is masked when logged.
  *
  * @param value the underlying path.
  */
case class MaskedPath(value: Path) extends ToLogString {

  /** @inheritdoc */
  override def toString: String =
    value.toAbsolutePath.normalize().toString

  /** @inheritdoc */
  override def toLogString(shouldMask: Boolean): String = {
    val path = value.toAbsolutePath.normalize()
    if (shouldMask) {
      MaskingUtils.toMaskedPath(path)
    } else {
      path.toString
    }
  }
}
