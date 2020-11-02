package org.enso.projectmanager.versionmanagement

import java.nio.file.Path
import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Stash}
import nl.gn0s1s.bump.SemVer
import org.enso.cli.{ProgressListener, TaskProgress}
import org.enso.projectmanager.data.{
  EngineVersion,
  MissingComponentAction,
  ProgressUnit
}
import org.enso.projectmanager.util.UnhandledLogging
import org.enso.runtimeversionmanager.components.{
  GraalVMVersion,
  RuntimeVersionManagementUserInterface,
  RuntimeVersionManager
}
import org.enso.runtimeversionmanager.releases.ReleaseProvider
import org.enso.runtimeversionmanager.releases.engine.EngineRepository
import org.enso.runtimeversionmanager.releases.graalvm.GraalCEReleaseProvider
import org.enso.runtimeversionmanager.runner.{JVMSettings, Runner}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/** Manages a [[RuntimeVersionManager]] instance.
  *
  * We put the [[RuntimeVersionManager]] into an Actor to ensure safety as it is
  * not designed for thread safety. As we currently do not need it to process
  * requests in parallel, this is the simpler thing to do rather than
  * implementing synchronization that may be brittle.
  *
  * Within each process, only one actor of this kind should be running at a
  * time.
  */
class RuntimeVersionManagerController
    extends Actor
    with ActorLogging
    with Stash
    with UnhandledLogging {
  import RuntimeVersionManagerController._

  private class ControllerInterface(
    user: ActorRef,
    allowBroken: Boolean,
    allowMissing: Boolean
  ) extends RuntimeVersionManagementUserInterface {
    override def trackProgress(task: TaskProgress[_]): Unit = {
      var uuid: Option[UUID] = None

      /** Initializes the task on first invocation and just returns the
        * generated UUID on further invocations.
        */
      def initializeTask(total: Option[Long]): UUID = uuid match {
        case Some(value) => value
        case None =>
          val generated = UUID.randomUUID()
          uuid = Some(generated)
          val unit = ProgressUnit.Other // TODO [RW] unit handling
          user ! Notification.TaskStarted(generated, total, unit)
          generated
      }
      task.addProgressListener(new ProgressListener[Any] {
        override def progressUpdate(
          done: Long,
          total: Option[Long]
        ): Unit = {
          val uuid = initializeTask(total)
          user ! Notification.TaskUpdate(uuid, done)
        }

        override def done(result: Try[Any]): Unit = result match {
          case Failure(exception) =>
            val uuid = initializeTask(None)
            user ! Notification.TaskFailure(uuid, exception)
          case Success(_) =>
            val uuid = initializeTask(None)
            user ! Notification.TaskSuccess(uuid)
        }
      })
    }

    override def shouldInstallMissingEngine(version: SemVer): Boolean =
      allowMissing

    override def shouldInstallMissingRuntime(
      version: GraalVMVersion
    ): Boolean = allowMissing

    override def shouldInstallBrokenEngine(version: SemVer): Boolean =
      allowBroken

    override def logInfo(message: => String): Unit = log.info(message)
  }

  private val engineProvider = EngineRepository.defaultEngineReleaseProvider

  private def runtimeVersionManager(
    user: ActorRef,
    allowMissing: Boolean = false,
    allowBroken: Boolean  = false
  ) = new RuntimeVersionManager(
    userInterface             = new ControllerInterface(user, allowBroken, allowMissing),
    distributionManager       = Managers.distributionManager,
    temporaryDirectoryManager = Managers.temporaryDirectoryManager,
    resourceManager           = Managers.resourceManager,
    engineReleaseProvider     = engineProvider,
    runtimeReleaseProvider    = GraalCEReleaseProvider
  )

  private def makeRunner(
    user: ActorRef,
    allowMissing: Boolean = false,
    allowBroken: Boolean  = false
  ) = new Runner(
    runtimeVersionManager =
      runtimeVersionManager(user, allowMissing, allowBroken),
    environment      = DefaultEnvironment,
    loggerConnection = Future.successful(None) // TODO [RW] logging in PM
  )

  private def makeRunner(
    user: ActorRef,
    missingComponentAction: MissingComponentAction
  ) = missingComponentAction match {
    case MissingComponentAction.Fail =>
      makeRunner(user, allowMissing = false, allowBroken = false)
    case MissingComponentAction.Install =>
      makeRunner(user, allowMissing = true, allowBroken = false)
    case MissingComponentAction.ForceInstallBroken =>
      makeRunner(user, allowMissing = true, allowBroken = true)
  }

  // TODO [RW] do we need a way to override this?
  private val jvmSettings = JVMSettings(false, Seq.empty)

  override def receive: Receive = defaultMode

  private def defaultMode: Receive = { case request: Request =>
    handleRequest(request)
  }

  private def supervisingLanguageServer: Receive = {
    case Request.StopLanguageServer =>
      stopLanguageServer()
      context.become(defaultMode)
    case _: Request => sender() ! Response.ServiceIsBusy
  }

  private def handleRequest(request: Request): Unit = request match {
    case Request.InstallEngine(version, forceInstallBroken) =>
      val result = Try {
        runtimeVersionManager(
          sender(),
          allowBroken  = forceInstallBroken,
          allowMissing = true
        ).findOrInstallEngine(version)
        Response.Success
      }
      respondWithTry(result)
    case Request.UninstallEngine(version) =>
      val result = Try {
        runtimeVersionManager(sender()).uninstallEngine(version)
        Response.Success
      }
      respondWithTry(result)
    case Request.ListInstalledEngines =>
      val result = Try {
        val list = runtimeVersionManager(sender())
          .listInstalledEngines()
          .map(engine =>
            EngineVersion(
              version        = engine.version,
              markedAsBroken = engine.isMarkedBroken
            )
          )
        Response.EngineList(list)
      }
      respondWithTry(result)
    case Request.ListAvailableEngines =>
      val result = engineProvider.fetchAllVersions().map { versions =>
        val list = versions.map {
          case ReleaseProvider.Version(version, markedAsBroken) =>
            EngineVersion(version, markedAsBroken)
        }
        Response.EngineList(list)
      }
      respondWithTry(result)
    case Request.CreateProject(
          path,
          name,
          version,
          authorName,
          authorEmail,
          additionalArguments,
          missingComponentAction
        ) =>
      def checkExitCode(exitCode: Int): Try[Unit] = if (exitCode == 0)
        Success(())
      else
        Failure(
          new RuntimeException(s"Engine returned non-zero exit code: $exitCode")
        )

      val runner = makeRunner(sender(), missingComponentAction)
      val result = for {
        runSettings <- runner.newProject(
          path                = path,
          name                = name,
          version             = version,
          authorName          = authorName,
          authorEmail         = authorEmail,
          additionalArguments = additionalArguments
        )
        exitCode <- runner.withCommand(runSettings, jvmSettings)(_.run())
        _        <- checkExitCode(exitCode)
      } yield Response.Success
      respondWithTry(result)

    case Request.StartLanguageServer(_) =>
      startLanguageServer()
  }

  private def respondWithTry(result: Try[Response]): Unit = result match {
    case Failure(exception) => sender() ! Response.Failure(exception)
    case Success(response)  => sender() ! response
  }

  private def startLanguageServer(): Unit = {
    context.become(supervisingLanguageServer)
  }

  private def stopLanguageServer(): Unit = {}
}

object RuntimeVersionManagerController {
  sealed trait Request
  object Request {
    case class InstallEngine(version: SemVer, forceInstallBroken: Boolean)
        extends Request
    case class UninstallEngine(version: SemVer) extends Request
    case object ListInstalledEngines            extends Request
    case object ListAvailableEngines            extends Request
    case class CreateProject(
      path: Path,
      name: String,
      version: SemVer,
      authorName: Option[String],
      authorEmail: Option[String],
      additionalArguments: Seq[String],
      missingComponentAction: MissingComponentAction
    )                                         extends Request
    case class StartLanguageServer(todo: Any) extends Request
    case object StopLanguageServer            extends Request
  }

  sealed trait Response
  object Response {

    /** Indicates that currently a managed Language Server instance is running
      * so other operations are not permitted.
      */
    case object ServiceIsBusy                          extends Response
    case object Success                                extends Response
    case class Failure(exception: Throwable)           extends Response
    case class EngineList(engines: Seq[EngineVersion]) extends Response
  }

  object Notification {
    case class TaskStarted(
      taskId: UUID,
      total: Option[Long],
      unit: ProgressUnit
    )
    case class TaskUpdate(taskId: UUID, done: Long)
    case class TaskSuccess(taskId: UUID)
    case class TaskFailure(taskId: UUID, throwable: Throwable)
  }
  def props: RuntimeVersionManagerController = ???
}
