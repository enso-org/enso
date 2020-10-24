package org.enso.languageserver.text

import org.enso.languageserver.filemanager.Path
import org.enso.text.editing.model.TextEdit

/** A representation of a batch of edits to a file, versioned.
  *
  * @param path a path of a file
  * @param edits a series of edits to a file
  * @param oldVersion the current version of a buffer
  * @param newVersion the version of a buffer after applying all edits
  */
case class FileEdit(
  path: Path,
  edits: List[TextEdit],
  oldVersion: Buffer.Version,
  newVersion: Buffer.Version
)
