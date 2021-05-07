package org.enso.logger
import java.nio.file.{FileSystems, Path}

object MaskingUtils {

  /** A substitution for the masked data. */
  final val STUB: String = "***"

  /** A synonym for `STUB`. */
  final val *** = STUB

  private val fileSeparator: String = FileSystems.getDefault.getSeparator

  /** Mask a path to the file
    *
    * @param path the file to mask
    * @return a string with a path to the file masked
    */
  def toMaskedPath(path: Path): String = {
    val segmentsCount = path.getNameCount
    if (segmentsCount > 1) {
      s"$STUB$fileSeparator${path.getFileName}"
    } else {
      STUB
    }
  }

}
