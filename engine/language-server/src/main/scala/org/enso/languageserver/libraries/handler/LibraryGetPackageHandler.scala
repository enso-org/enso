package org.enso.languageserver.libraries.handler

import akka.actor.{Actor, ActorRef, Cancellable, Props, Status}
import akka.pattern.pipe
import com.typesafe.scalalogging.LazyLogging
import nl.gn0s1s.bump.SemVer
import org.enso.editions.Editions.Repository
import org.enso.editions.LibraryName
import org.enso.jsonrpc._
import org.enso.languageserver.libraries.LibraryApi._
import org.enso.languageserver.libraries._
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.util.UnhandledLogging
import org.enso.librarymanager.published.PublishedLibraryCache
import org.enso.librarymanager.published.repository.RepositoryHelper.RepositoryMethods

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

/** A request handler for the `library/getPackage` endpoint.
  *
  * @param timeout request timeout
  * @param localLibraryManager reference to the local library manager actor
  * @param publishedLibraryCache the cache of published libraries
  */
class LibraryGetPackageHandler(
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
          LibraryGetPackage,
          id,
          LibraryGetPackage.Params(namespace, name, version)
        ) =>
      val libraryName = LibraryName(namespace, name)
      version match {
        case LibraryEntry.LocalLibraryVersion =>
          localLibraryManager ! LocalLibraryManagerProtocol.GetPackage(
            libraryName
          )
        case LibraryEntry.PublishedLibraryVersion(version, repositoryUrl) =>
          SemVer(version) match {
            case Some(semVerVersion) =>
              getOrFetchPublishedPackage(
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

    case LocalLibraryManagerProtocol.GetPackageResponse(
          libraryName,
          license,
          componentGroups,
          rawPackage
        ) =>
      replyTo ! ResponseResult(
        LibraryGetPackage,
        id,
        LibraryGetPackage.Result(
          Option.unless(license.isEmpty)(license),
          componentGroups.flatMap { groups =>
            Option.unless(
              groups.newGroups.isEmpty && groups.extendedGroups.isEmpty
            )(LibraryComponentGroups.fromComponentGroups(libraryName, groups))
          },
          rawPackage
        )
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

  private def getOrFetchPublishedPackage(
    libraryName: LibraryName,
    version: SemVer,
    repositoryUrl: String
  ): Future[LocalLibraryManagerProtocol.GetPackageResponse] =
    getCachedPackage(libraryName, version) match {
      case Some(response) =>
        response.fold(Future.failed, Future.successful)
      case None =>
        fetchPublishedPackage(libraryName, version, repositoryUrl)
    }

  private def getCachedPackage(
    libraryName: LibraryName,
    version: SemVer
  ): Option[Try[LocalLibraryManagerProtocol.GetPackageResponse]] =
    publishedLibraryCache
      .findCachedLibrary(libraryName, version)
      .map { libraryPath =>
        libraryPath.getReadAccess
          .readPackage()
          .map(config =>
            LocalLibraryManagerProtocol.GetPackageResponse(
              LibraryName(config.namespace, config.name),
              config.license,
              config.componentGroups.toOption,
              config.originalJson
            )
          )
      }

  private def fetchPublishedPackage(
    libraryName: LibraryName,
    version: SemVer,
    repositoryUrl: String
  ): Future[LocalLibraryManagerProtocol.GetPackageResponse] = for {
    config <- Repository(repositoryUrl)
      .accessLibrary(libraryName, version)
      .fetchPackageConfig()
      .toFuture
  } yield LocalLibraryManagerProtocol.GetPackageResponse(
    libraryName     = LibraryName(config.namespace, config.name),
    license         = config.license,
    componentGroups = config.componentGroups.toOption,
    rawPackage      = config.originalJson
  )
}

object LibraryGetPackageHandler {

  /** Creates a configuration object to create [[LibraryGetPackageHandler]].
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
      new LibraryGetPackageHandler(
        timeout,
        localLibraryManager,
        publishedLibraryCache
      )
    )
}
