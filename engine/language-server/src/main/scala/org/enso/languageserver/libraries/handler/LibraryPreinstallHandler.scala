package org.enso.languageserver.libraries.handler

import akka.actor.{Actor, ActorRef, Props, Status}
import akka.pattern.pipe
import cats.implicits.toTraverseOps
import com.typesafe.scalalogging.LazyLogging
import org.enso.cli.task.notifications.ActorProgressNotificationForwarder
import org.enso.cli.task.{
  ProgressNotification,
  ProgressReporter,
  TaskProgressImplementation
}
import org.enso.distribution.ProgressAndLockNotificationForwarder
import org.enso.distribution.locking.LockUserInterface
import org.enso.editions.LibraryName
import org.enso.jsonrpc.{Id, Request, ResponseError, ResponseResult, Unused}
import org.enso.languageserver.filemanager.FileManagerApi.FileSystemError
import org.enso.languageserver.libraries.LibraryApi._
import org.enso.languageserver.libraries.handler.LibraryPreinstallHandler.{
  DependencyGatheringError,
  InstallationError,
  InstallationResult,
  InstallerError,
  InternalError
}
import org.enso.languageserver.libraries.{
  EditionReference,
  EditionReferenceResolver,
  LibraryConfig
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.librarymanager.ResolvingLibraryProvider.Error
import org.enso.librarymanager.dependencies.{Dependency, DependencyResolver}
import org.enso.librarymanager.{
  DefaultLibraryProvider,
  LibraryResolver,
  ResolvedLibrary,
  ResolvingLibraryProvider
}

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

/** A request handler for the `library/preinstall` endpoint.
  *
  * This request handler does not have any timeouts, because the download can
  * take a very long time, highly depending on the library being downloaded
  * (some libraries can be huge) and the network speed, so there is no good way
  * to select a reasonable timeout.
  *
  * @param editionReferenceResolver an [[EditionReferenceResolver]] instance
  * @param config configuration for the library subsystem
  */
class LibraryPreinstallHandler(
  editionReferenceResolver: EditionReferenceResolver,
  config: LibraryConfig
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  private val threadPool: ExecutorService = Executors.newCachedThreadPool()
  implicit private val ec: ExecutionContext =
    ExecutionContext.fromExecutor(threadPool)

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

      val installation: Future[InstallationResult] =
        installLibraryWithDependencies(libraryName, notificationForwarder)
      installation pipeTo self

      context.become(responseStage(id, replyTo, libraryName))
  }

  /** Returns a future that will be completed once all dependencies of the
    * library have been installed.
    *
    * @param libraryName name of the library to install
    * @param notificationForwarder a notification handler for reporting progress
    */
  private def installLibraryWithDependencies(
    libraryName: LibraryName,
    notificationForwarder: ProgressAndLockNotificationForwarder
  ): Future[InstallationResult] = Future {
    val result = for {
      tools <- instantiateTools(notificationForwarder).toEither.left
        .map(InternalError)
      dependencies <- tools.dependencyResolver
        .findDependencies(libraryName)
        .toEither
        .left
        .map(DependencyGatheringError)
      dependenciesToInstall = dependencies.filter(!_.isCached)
      _ <- installDependencies(
        dependenciesToInstall,
        notificationForwarder,
        tools.libraryInstaller
      )
      library <- tools.libraryInstaller
        .findLibrary(libraryName)
        .left
        .map(InstallerError)
    } yield library
    InstallationResult(result)
  }

  /** Installs the provided dependencies and reports the overall progress. */
  private def installDependencies(
    dependencies: Set[Dependency],
    notificationForwarder: ProgressAndLockNotificationForwarder,
    libraryInstaller: ResolvingLibraryProvider
  ): Either[InstallationError, Unit] = {

    logger.trace(s"Dependencies to install: $dependencies.")

    val taskProgress = new TaskProgressImplementation[Unit]()

    val message =
      if (dependencies.size == 1) s"Installing 1 library."
      else s"Installing ${dependencies.size} libraries."

    notificationForwarder.trackProgress(
      message,
      taskProgress
    )

    val total = Some(dependencies.size.toLong)
    taskProgress.reportProgress(0, total)

    val results =
      dependencies.toList.zipWithIndex.traverse { case (dependency, ix) =>
        val result = libraryInstaller.findSpecificLibraryVersion(
          dependency.libraryName,
          dependency.version
        )

        taskProgress.reportProgress(ix.toLong + 1, total)
        result
      }

    taskProgress.setComplete(Success(()))

    results.map { _ => () }.left.map(InstallerError)
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
              FileSystemError(s"Internal error: ${throwable.toString}")
            case DependencyGatheringError(throwable) =>
              DependencyDiscoveryError(throwable.toString)
            case InstallerError(Error.NotResolved(_)) =>
              LibraryNotResolved(libraryName)
            case InstallerError(Error.RequestedLocalLibraryDoesNotExist) =>
              LocalLibraryNotFound(libraryName)
            case InstallerError(Error.DownloadFailed(version, reason)) =>
              LibraryDownloadError(libraryName, version, reason.toString)
          }
          replyTo ! ResponseError(
            Some(requestId),
            errorMessage
          )
        case Right(_) =>
          replyTo ! ResponseResult(LibraryPreinstall, requestId, Unused)
      }

      threadPool.shutdown()
      context.stop(self)

    case Status.Failure(throwable) =>
      self ! Left(InternalError(throwable))
  }

  case class Tools(
    libraryInstaller: ResolvingLibraryProvider,
    dependencyResolver: DependencyResolver
  )

  /** A helper function that creates instances if the library installer and
    * dependency resolver that report to the provided notification forwarder.
    */
  private def instantiateTools(
    notificationReporter: ProgressReporter with LockUserInterface
  ): Try[Tools] =
    for {
      projectConfig <- editionReferenceResolver.getCurrentProjectConfig
      edition <- editionReferenceResolver.resolveEdition(
        EditionReference.CurrentProjectEdition
      )
      preferLocalLibraries = projectConfig.preferLocalLibraries
      installer = DefaultLibraryProvider.make(
        distributionManager  = config.installerConfig.distributionManager,
        resourceManager      = config.installerConfig.resourceManager,
        lockUserInterface    = notificationReporter,
        progressReporter     = notificationReporter,
        languageHome         = config.installerConfig.languageHome,
        edition              = edition,
        preferLocalLibraries = preferLocalLibraries,
        projectRoot          = Some(editionReferenceResolver.projectRoot.toPath)
      )
      dependencyResolver = new DependencyResolver(
        localLibraryProvider     = config.localLibraryProvider,
        publishedLibraryProvider = config.publishedLibraryCache,
        edition                  = edition,
        preferLocalLibraries     = preferLocalLibraries,
        libraryResolver          = LibraryResolver(config.localLibraryProvider),
        dependencyExtractor      = config.installerConfig.dependencyExtractor
      )
    } yield Tools(installer, dependencyResolver)
}

object LibraryPreinstallHandler {

  /** Creates a configuration object to create [[LibraryPreinstallHandler]].
    *
    * @param editionReferenceResolver an [[EditionReferenceResolver]] instance
    * @param config configuration for the library subsystem
    */
  def props(
    editionReferenceResolver: EditionReferenceResolver,
    config: LibraryConfig
  ): Props = Props(
    new LibraryPreinstallHandler(editionReferenceResolver, config)
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

  /** Indicates an error that occurred when looking for all of the transitive
    * dependencies of the library.
    */
  case class DependencyGatheringError(throwable: Throwable)
      extends InstallationError
}
