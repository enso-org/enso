package org.enso.languageserver.text

import java.io.File
import java.time.OffsetDateTime

/** A file with extra info.
  *
  * @param file the underlying file
  * @param lastModifiedTime the last known modified time on disk
  */
case class FileWithMetadata(
  file: File,
  lastModifiedTime: Option[OffsetDateTime]
)
object FileWithMetadata {

  /** Create a file with metadata.
    *
    * @param file the underlying file
    * @return a new instance of [[FileWithMetadata]]
    */
  def apply(file: File): FileWithMetadata =
    FileWithMetadata(file, None)
}
