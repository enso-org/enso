package org.enso.languageserver.filemanager

import java.io.File
import java.util.UUID

object ContentRootManagerProtocol {

  /** Request to get all currently registered content roots. */
  case object GetContentRoots

  /** Response containing all current content roots. */
  case class GetContentRootsResult(contentRoots: List[ContentRootWithFile])

  /** Request to find the content root with the given id. */
  case class FindContentRoot(id: UUID)

  /** Response to [[FindContentRoot]] containing the found content root or None.
    */
  case class FindContentRootResult(contentRoot: Option[ContentRootWithFile])

  /** Request to resolve the path as relative to one of the registered content
    * roots.
    */
  case class FindRelativePath(path: File)

  /** Response to [[FindRelativePath]] containing the relativized path or None
    * if the path did not match any registered content root.
    */
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
    *
    * Due to the synchronization issue sketched above, the initial set of
    * content roots should be inferred from the first notification and not from
    * [[GetContentRoots]]. [[GetContentRoots]] should only be used if a
    * one-time, possibly immediately outdated, information about current content
    * root is needed.
    */
  case object SubscribeToNotifications

  /** The notification sent to confirm the subscription and then for every added content root to every subscriber.
    *
    * @param newRoots the list of roots that were added since the last time that
    *                 notification has been sent (or all currently registered
    *                 roots when it is sent for the first time)
    */
  case class ContentRootsAddedNotification(newRoots: List[ContentRootWithFile])
}
