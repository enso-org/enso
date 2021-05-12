package org.enso.logger.masking

import java.nio.file.Path

/** A path that is masked when logged.
  *
  * @param value the underlying path.
  */
case class MaskedPath(value: Path) extends ToMaskedString {

  /** @inheritdoc */
  override def toString: String =
    value.toAbsolutePath.normalize().toString

  /** @inheritdoc */
  override def toMaskedString: String =
    MaskingUtils.toMaskedPath(value.toAbsolutePath.normalize())
}
