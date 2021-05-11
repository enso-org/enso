package org.enso.logger.masking

import java.nio.file.Path

/** A path that is masked when logged.
  *
  * @param path the underlying path.
  */
case class MaskedPath(path: Path) extends ToMaskedString {

  /** @inheritdoc */
  override def toMaskedString: String =
    MaskingUtils.toMaskedPath(path.toAbsolutePath.normalize())
}
