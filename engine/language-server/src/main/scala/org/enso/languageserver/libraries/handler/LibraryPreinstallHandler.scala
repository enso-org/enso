package org.enso.languageserver.libraries.handler

import akka.actor.{Actor, ActorRef, Props, Status}
import akka.pattern.pipe
import com.typesafe.scalalogging.LazyLogging
import org.enso.cli.task.notifications.ActorProgressNotificationForwarder
import org.enso.cli.task.{ProgressNotification, ProgressReporter}
import org.enso.distribution.ProgressAndLockNotificationForwarder
import org.enso.distribution.locking.LockUserInterface
import org.enso.editions.LibraryName
import org.enso.jsonrpc.{Id, Request, ResponseError, ResponseResult, Unused}
import org.enso.languageserver.filemanager.FileManagerApi.FileSystemError
import org.enso.languageserver.libraries.LibraryApi._
import org.enso.languageserver.libraries.handler.LibraryPreinstallHandler.{
  InstallationResult,
  InstallerError,
  InternalError
}
import org.enso.languageserver.libraries.{
  EditionReference,
  EditionReferenceResolver,
  LibraryInstallerConfig
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.librarymanager.ResolvingLibraryProvider.Error
import org.enso.librarymanager.{
  DefaultLibraryProvider,
  ResolvedLibrary,
  ResolvingLibraryProvider
}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/** A request handler for the `library/preinstall` endpoint.
  *
  * This request handler does not have any timeouts, because the download can
  * take a very long time, highly depending on the library being downloaded
  * (some libraries can be huge) and the network speed, so there is no good way
  * to select a reasonable timeout.
  *
  * @param editionReferenceResolver an [[EditionReferenceResolver]] instance
  * @param installerConfig configuration for the library installer
  */
class LibraryPreinstallHandler(
  editionReferenceResolver: EditionReferenceResolver,
  installerConfig: LibraryInstallerConfig
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  implicit private val ec: ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(
          LibraryPreinstall,
          id,
          LibraryPreinstall.Params(namespace, name)
        ) =>
      val replyTo     = sender()
      val libraryName = LibraryName(namespace, name)
      val notificationForwarder = new ProgressAndLockNotificationForwarder {
        override def sendProgressNotification(
          notification: ProgressNotification
        ): Unit =
          replyTo ! ActorProgressNotificationForwarder
            .translateProgressNotification(LibraryPreinstall.name, notification)
      }

      val installation: Future[InstallationResult] = Future {
        val result = for {
          libraryInstaller <- getLibraryProvider(
            notificationForwarder
          ).toEither.left.map(InternalError)
          library <- libraryInstaller
            .findLibrary(libraryName)
            .left
            .map(InstallerError)
        } yield library
        InstallationResult(result)
      }
      installation pipeTo self

      context.become(responseStage(id, replyTo, libraryName))
  }

  private def responseStage(
    requestId: Id,
    replyTo: ActorRef,
    libraryName: LibraryName
  ): Receive = {
    case InstallationResult(result) =>
      result match {
        case Left(error) =>
          val errorMessage = error match {
            case InternalError(throwable) =>
              FileSystemError(s"Internal error: ${throwable.getMessage}")
            case InstallerError(Error.NotResolved(_)) =>
              LibraryNotResolved(libraryName)
            case InstallerError(Error.RequestedLocalLibraryDoesNotExist) =>
              LocalLibraryNotFound(libraryName)
            case InstallerError(Error.DownloadFailed(version, reason)) =>
              LibraryDownloadError(libraryName, version, reason.getMessage)
          }
          replyTo ! ResponseError(
            Some(requestId),
            errorMessage
          )
        case Right(_) =>
          replyTo ! ResponseResult(LibraryPreinstall, requestId, Unused)
      }

      context.stop(self)

    case Status.Failure(throwable) =>
      self ! Left(InternalError(throwable))
  }

  private def getLibraryProvider(
    notificationReporter: ProgressReporter with LockUserInterface
  ): Try[ResolvingLibraryProvider] =
    for {
      config <- editionReferenceResolver.getCurrentProjectConfig
      edition <- editionReferenceResolver.resolveEdition(
        EditionReference.CurrentProjectEdition
      )
    } yield DefaultLibraryProvider.make(
      distributionManager  = installerConfig.distributionManager,
      resourceManager      = installerConfig.resourceManager,
      lockUserInterface    = notificationReporter,
      progressReporter     = notificationReporter,
      languageHome         = installerConfig.languageHome,
      edition              = edition,
      preferLocalLibraries = config.preferLocalLibraries
    )
}

object LibraryPreinstallHandler {

  /** Creates a configuration object to create [[LibraryPreinstallHandler]].
    *
    * @param editionReferenceResolver an [[EditionReferenceResolver]] instance
    * @param installerConfig configuration for the library installer
    */
  def props(
    editionReferenceResolver: EditionReferenceResolver,
    installerConfig: LibraryInstallerConfig
  ): Props = Props(
    new LibraryPreinstallHandler(editionReferenceResolver, installerConfig)
  )

  /** An internal message used to pass the installation result from the Future
    * back to the Actor.
    *
    * It is used, because a pattern match directly on the [[Either]] would be
    * unchecked due to type erasure.
    */
  case class InstallationResult(
    result: Either[InstallationError, ResolvedLibrary]
  )

  /** Indicates any error that happened during the installation. */
  sealed trait InstallationError

  /** Indicates an internal error which means that the installer could not even
    * be instantiated.
    *
    * These may include things like not being able to load current project
    * configuration to deduce the edition to use for resolving the requested
    * library version.
    */
  case class InternalError(throwable: Throwable) extends InstallationError

  /** Indicates a more casual error that has happened during the installation -
    * for example that the library was not found or that the network connection
    * could not be established.
    */
  case class InstallerError(error: Error) extends InstallationError
}
