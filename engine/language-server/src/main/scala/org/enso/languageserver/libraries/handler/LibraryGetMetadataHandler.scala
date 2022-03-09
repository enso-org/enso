package org.enso.languageserver.libraries.handler

import akka.actor.{Actor, ActorRef, Cancellable, Props, Status}

import scala.util.{Success, Try}
import akka.pattern.pipe
import com.typesafe.scalalogging.LazyLogging
import nl.gn0s1s.bump.SemVer
import org.enso.editions.Editions.Repository
import org.enso.editions.LibraryName
import org.enso.jsonrpc._
import org.enso.languageserver.libraries.LibraryApi._
import org.enso.languageserver.libraries.{
  LibraryEntry,
  LocalLibraryManagerFailureMapper,
  LocalLibraryManagerProtocol
}
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.util.UnhandledLogging
import org.enso.librarymanager.published.PublishedLibraryCache
import org.enso.librarymanager.published.repository.RepositoryHelper.RepositoryMethods

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

/** A request handler for the `library/getMetadata` endpoint.
  *
  * @param timeout request timeout
  * @param localLibraryManager reference to the local library manager actor
  * @param publishedLibraryCache the cache of published libraries
  */
class LibraryGetMetadataHandler(
  timeout: FiniteDuration,
  localLibraryManager: ActorRef,
  publishedLibraryCache: PublishedLibraryCache
) extends Actor
    with LazyLogging
    with UnhandledLogging {
  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(
          LibraryGetMetadata,
          id,
          LibraryGetMetadata.Params(namespace, name, version)
        ) =>
      val libraryName = LibraryName(namespace, name)
      version match {
        case LibraryEntry.LocalLibraryVersion =>
          localLibraryManager ! LocalLibraryManagerProtocol.GetMetadata(
            libraryName
          )
        case LibraryEntry.PublishedLibraryVersion(version, repositoryUrl) =>
          SemVer(version) match {
            case Some(semVerVersion) =>
              getOrFetchPublishedMetadata(
                libraryName,
                semVerVersion,
                repositoryUrl
              ) pipeTo self
            case None =>
              self ! LocalLibraryManagerProtocol.InvalidSemverVersionError(
                version
              )
          }
      }

      val cancellable =
        context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(responseStage(id, sender(), cancellable))
  }

  private def responseStage(
    id: Id,
    replyTo: ActorRef,
    cancellable: Cancellable
  ): Receive = {
    case RequestTimeout =>
      logger.error("Request [{}] timed out.", id)
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case LocalLibraryManagerProtocol.GetMetadataResponse(
          description,
          tagLine
        ) =>
      replyTo ! ResponseResult(
        LibraryGetMetadata,
        id,
        LibraryGetMetadata.Result(description, tagLine)
      )
      cancellable.cancel()
      context.stop(self)

    case failure: LocalLibraryManagerProtocol.Failure =>
      replyTo ! LocalLibraryManagerFailureMapper.mapFailure(failure)
      cancellable.cancel()
      context.stop(self)

    case Status.Failure(exception) =>
      replyTo ! ResponseError(
        Some(id),
        LocalLibraryManagerFailureMapper.mapException(exception)
      )
      cancellable.cancel()
      context.stop(self)
  }

  private def getOrFetchPublishedMetadata(
    libraryName: LibraryName,
    version: SemVer,
    repositoryUrl: String
  ): Future[LocalLibraryManagerProtocol.GetMetadataResponse] =
    getCachedMetadata(libraryName, version) match {
      case Some(response) =>
        response.fold(Future.failed, Future.successful)
      case None =>
        fetchPublishedMetadata(libraryName, version, repositoryUrl)
    }

  private def getCachedMetadata(
    libraryName: LibraryName,
    version: SemVer
  ): Option[Try[LocalLibraryManagerProtocol.GetMetadataResponse]] =
    publishedLibraryCache
      .findCachedLibrary(libraryName, version)
      .map { libraryPath =>
        libraryPath.getReadAccess
          .readManifest()
          .map { manifestAttempt =>
            manifestAttempt.map(manifest =>
              LocalLibraryManagerProtocol.GetMetadataResponse(
                manifest.description,
                manifest.tagLine
              )
            )
          }
          .getOrElse(
            Success(LocalLibraryManagerProtocol.GetMetadataResponse(None, None))
          )
      }

  private def fetchPublishedMetadata(
    libraryName: LibraryName,
    version: SemVer,
    repositoryUrl: String
  ): Future[LocalLibraryManagerProtocol.GetMetadataResponse] = for {
    manifest <- Repository(repositoryUrl)
      .accessLibrary(libraryName, version)
      .fetchManifest()
      .toFuture
  } yield LocalLibraryManagerProtocol.GetMetadataResponse(
    description = manifest.description,
    tagLine     = manifest.tagLine
  )
}

object LibraryGetMetadataHandler {

  /** Creates a configuration object to create [[LibraryGetMetadataHandler]].
    *
    * @param timeout request timeout
    * @param localLibraryManager reference to the local library manager actor
    * @param publishedLibraryCache the cache of published libraries
    */
  def props(
    timeout: FiniteDuration,
    localLibraryManager: ActorRef,
    publishedLibraryCache: PublishedLibraryCache
  ): Props =
    Props(
      new LibraryGetMetadataHandler(
        timeout,
        localLibraryManager,
        publishedLibraryCache
      )
    )
}
