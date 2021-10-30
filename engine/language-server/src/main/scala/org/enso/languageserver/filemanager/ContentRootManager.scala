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

  /** Lists the content roots currently available.
    *
    * @param ec the execution context on which to wait for the reply from the
    *           content root manager
    * @return a future that will complete with a list of currently available
    *         content roots
    */
  def getContentRoots(implicit
    ec: ExecutionContext
  ): Future[List[ContentRootWithFile]]

  /** Finds the content root with the given id.
    *
    * @param id the id of the content root to get
    * @param ec the execution context on which to wait for the reply from the
    *           content root manager
    * @return a future that will complete either with the found content root or
    *         with an error if a content root with the requested id was not
    *         found
    */
  def findContentRoot(id: UUID)(implicit
    ec: ExecutionContext
  ): Future[Either[ContentRootNotFound.type, ContentRootWithFile]]

  /** Converts a filesystem path to a path relative to one of the available
    * content roots.
    *
    * The implementation should pick the most specific content root for the
    * path, i.e. the one whose root location shares the longest common prefix
    * with the path that is being resolved.
    *
    * @param path the path to convert
    * @param ec the execution context on which to wait for the reply from the
    *           content root manager
    * @return a future that will complete with the relativized path or None if
    *         the path was not relative to any of the available roots
    */
  def findRelativePath(path: File)(implicit
    ec: ExecutionContext
  ): Future[Option[Path]]
}
