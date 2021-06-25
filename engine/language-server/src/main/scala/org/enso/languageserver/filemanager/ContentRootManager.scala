package org.enso.languageserver.filemanager

import java.io.File
import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

/** The external interface of the Content Root Manager that can be used by other
  * components.
  *
  * This interface encapsulates the underlying Actor-based implementation and
  * only exposes the operations that are simple requests. To subscribe for
  * content root updates, the Actor interface should be used directly.
  */
trait ContentRootManager {

  /** Lists the content roots currently available. */
  def getContentRoots(implicit
    ec: ExecutionContext
  ): Future[List[ContentRootWithFile]]

  /** Finds the content root with the given id. */
  def findContentRoot(id: UUID)(implicit
    ec: ExecutionContext
  ): Future[Either[ContentRootNotFound.type, ContentRootWithFile]]

  /** Converts a filesystem path to a path relative to one of the available
    * content roots.
    *
    * The implementation should pick the most specific content root for the
    * path, i.e. the one whose root location shares the longest common prefix
    * with the path that is being resolved.
    */
  def findRelativePath(path: File)(implicit
    ec: ExecutionContext
  ): Future[Option[Path]]
}
