package org.enso.languageserver.filemanager

import java.io.File
import java.util.UUID

object ContentRootManagerProtocol {

  /** Gets all content roots. */
  case object GetContentRoots

  /** Response containing all current content roots. */
  case class GetContentRootsResult(contentRoots: List[ContentRootWithFile])

  case class AddLibraryRoot(todo: Any) // TODO

  case class FindContentRoot(id: UUID)
  case class FindContentRootResult(contentRoot: Option[ContentRootWithFile])

  case class FindRelativePath(path: File)
  case class FindRelativePathResult(path: Option[Path])
}
