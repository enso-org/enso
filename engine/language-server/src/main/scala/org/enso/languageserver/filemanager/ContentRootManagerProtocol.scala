package org.enso.languageserver.filemanager

import java.io.File
import java.util.UUID

object ContentRootManagerProtocol {

  /** Gets all content roots. */
  case object GetContentRoots

  /** Response containing all current content roots. */
  case class GetContentRootsResult(contentRoots: List[ContentRootWithFile])

  case class FindContentRoot(id: UUID)
  case class FindContentRootResult(contentRoot: Option[ContentRootWithFile])

  case class FindRelativePath(path: File)
  case class FindRelativePathResult(path: Option[Path])

  /** The message that should be sent t the [[ContentRootManagerActor]] to
    * subscribe to updates about new content roots.
    *
    * When this message is received, a message with the current set of content
    * root is sent immediately as a reply. This is done to avoid synchronization
    * issues that could occur if only new roots were being reported (a possible
    * race condition if a new root is added between a [[GetContentRoots]] and
    * [[SubscribeToNotifications]] messages that would cause this new root to
    * not be noticed by the subscriber; in the scenario that on subscription all
    * current roots are sent, this is not an issue anymore). This message is
    * sent even if there are no registered content roots (although this should
    * never happen in practice, as the project root should always be present),
    * to confirm the subscription .
    */
  case object SubscribeToNotifications
  case class ContentRootsAddedNotification(newRoots: List[ContentRootWithFile])
}
