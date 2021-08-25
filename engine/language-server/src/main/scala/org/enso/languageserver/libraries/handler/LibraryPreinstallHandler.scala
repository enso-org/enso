package org.enso.languageserver.libraries.handler

import akka.actor.{Actor, ActorRef, Props}
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
  InstallationError,
  InstallerError,
  InternalError,
  LibraryInstallationComplete
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

  override def receive: Receive = {
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

      val installation = Future {
        val result: Either[InstallationError, ResolvedLibrary] = for {
          libraryInstaller <- getLibraryProvider(
            notificationForwarder
          ).toEither.left.map(InternalError)
          library <- libraryInstaller
            .findLibrary(libraryName)
            .left
            .map(InstallerError)
        } yield library
        LibraryInstallationComplete(id, replyTo, libraryName, result)
      }
      installation pipeTo self

    case LibraryInstallationComplete(requestId, replyTo, libraryName, result) =>
      result match {
        case Left(error) =>
          val errorMessage = error match {
            case InternalError(throwable) =>
              FileSystemError(
                s"Could not initialize library installer: " +
                s"${throwable.getMessage}"
              )
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

  case class LibraryInstallationComplete(
    requestId: Id,
    replyTo: ActorRef,
    libraryName: LibraryName,
    result: Either[InstallationError, ResolvedLibrary]
  )

  sealed trait InstallationError
  case class InternalError(throwable: Throwable) extends InstallationError
  case class InstallerError(error: Error)        extends InstallationError
}
