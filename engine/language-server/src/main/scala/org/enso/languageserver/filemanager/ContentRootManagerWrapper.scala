package org.enso.languageserver.filemanager

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import org.enso.languageserver.data.Config
import org.enso.languageserver.filemanager.ContentRootManagerProtocol._

import java.io.File
import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

class ContentRootManagerWrapper(
  config: Config,
  contentRootManagerActor: ActorRef
) extends ContentRootManager {
  implicit val timeout: Timeout = Timeout(
    2 * config.executionContext.requestTimeout
  )

  override def getContentRoots(implicit
    ec: ExecutionContext
  ): Future[List[ContentRootWithFile]] =
    (contentRootManagerActor ? GetContentRoots)
      .mapTo[GetContentRootsResult]
      .map(_.contentRoots)

  override def findContentRoot(
    id: UUID
  )(implicit ec: ExecutionContext): Future[ContentRootWithFile] =
    (contentRootManagerActor ? FindContentRoot(id))
      .mapTo[FindContentRootResult]
      .flatMap {
        case FindContentRootResult(Some(root)) =>
          Future.successful(root)
        case FindContentRootResult(None) =>
          Future.failed(ContentRootManager.ContentRootNotFound(id))
      }

  override def findRelativePath(
    path: File
  )(implicit ec: ExecutionContext): Future[Option[Path]] =
    (contentRootManagerActor ? FindRelativePath(path))
      .mapTo[FindRelativePathResult]
      .map(_.path)
}
