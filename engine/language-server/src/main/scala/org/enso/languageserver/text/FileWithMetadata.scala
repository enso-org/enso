package org.enso.languageserver.text

import java.io.File
import java.time.OffsetDateTime

/** A file with extra info.
  *
  * @param file the underlying file
  * @param isModifiedOnDisk is the file modified on disk
  * @param lastModifiedTime the last known modified time on disk
  */
case class FileWithMetadata(
  file: File,
  isModifiedOnDisk: Boolean                = false,
  lastModifiedTime: Option[OffsetDateTime] = None
)
