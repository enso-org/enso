package org.enso.languageserver.filemanager

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import org.enso.languageserver.data.Config
import org.enso.languageserver.filemanager.ContentRootManagerProtocol._

import java.io.File
import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

/** An implementation of [[ContentRootManager]] that wraps the
  * [[ContentRootManagerActor]] in an interface that is more friendly for non-Actor-based clients.
  */
class ContentRootManagerWrapper(
  config: Config,
  contentRootManagerActor: ActorRef
) extends ContentRootManager {
  implicit private val timeout: Timeout = Timeout(
    2 * config.executionContext.requestTimeout
  )

  /** @inheritdoc */
  override def getContentRoots(implicit
    ec: ExecutionContext
  ): Future[List[ContentRootWithFile]] =
    (contentRootManagerActor ? GetContentRoots)
      .mapTo[GetContentRootsResult]
      .map(_.contentRoots)

  /** @inheritdoc */
  override def findContentRoot(id: UUID)(implicit
    ec: ExecutionContext
  ): Future[Either[ContentRootNotFound.type, ContentRootWithFile]] =
    (contentRootManagerActor ? FindContentRoot(id))
      .mapTo[FindContentRootResult]
      .map {
        case FindContentRootResult(Some(root)) =>
          Right(root)
        case FindContentRootResult(None) =>
          Left(ContentRootNotFound)
      }
      .recoverWith { _ =>
        Future.successful(Left(ContentRootNotFound))
      }

  /** @inheritdoc */
  override def findRelativePath(
    path: File
  )(implicit ec: ExecutionContext): Future[Option[Path]] =
    (contentRootManagerActor ? FindRelativePath(path))
      .mapTo[FindRelativePathResult]
      .map(_.path)
}
