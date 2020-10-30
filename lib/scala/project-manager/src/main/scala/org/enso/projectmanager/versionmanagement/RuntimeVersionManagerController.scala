package org.enso.projectmanager.versionmanagement

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Stash}
import nl.gn0s1s.bump.SemVer
import org.enso.cli.{ProgressListener, TaskProgress}
import org.enso.projectmanager.data.{EngineVersion, ProgressUnit}
import org.enso.projectmanager.util.UnhandledLogging
import org.enso.runtimeversionmanager.components.{
  GraalVMVersion,
  RuntimeVersionManagementUserInterface,
  RuntimeVersionManager
}
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

  private var allowBroken                   = false
  private var allowMissing                  = false
  private var currentUser: Option[ActorRef] = None
  private val interface = new RuntimeVersionManagementUserInterface {
    override def trackProgress(task: TaskProgress[_]): Unit = {
      currentUser match {
        case Some(user) =>
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
        case None =>
          log.warning("Progress reported but no current user.")
      }
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

  private val runtimeVersionManager = new RuntimeVersionManager(
    userInterface             = interface,
    distributionManager       = Managers.distributionManager,
    temporaryDirectoryManager = Managers.temporaryDirectoryManager,
    resourceManager           = Managers.resourceManager,
    engineReleaseProvider     = engineProvider,
    runtimeReleaseProvider    = GraalCEReleaseProvider
  )

  private val runner = new Runner(
    runtimeVersionManager = runtimeVersionManager,
    environment           = DefaultEnvironment,
    loggerConnection      = Future.successful(None) // TODO [RW] logging in PM
  )

  // TODO [RW] do we need a way to override this?
  private val jvmSettings = JVMSettings(false, Seq.empty)

  override def receive: Receive = { case request: Request =>
    currentUser = Some(sender())
    handleRequest(request)
  }

  private def handleRequest(request: Request): Unit = request match {
    case Request.InstallEngine(version, forceInstallBroken) =>
      allowBroken  = forceInstallBroken
      allowMissing = true
      try {
        runtimeVersionManager.findOrInstallEngine(version)
        sender() ! Response.Success
      } catch {
        case error: Exception =>
          sender() ! Response.Failure(error)
      }
    case Request.UninstallEngine(version) =>
      try {
        runtimeVersionManager.uninstallEngine(version)
        sender() ! Response.Success
      } catch {
        case error: Exception =>
          sender() ! Response.Failure(error)
      }
    case Request.ListInstalledEngines =>
      val list = runtimeVersionManager
        .listInstalledEngines()
        .map(engine =>
          EngineVersion(
            version        = engine.version,
            markedAsBroken = engine.isMarkedBroken
          )
        )
      sender() ! Response.EngineList(list)
    case Request.ListAvailableEngines =>
      // TODO [RW, AO] the remote provider only returns non-broken versions,
      //  is that ok?
      engineProvider.fetchAllValidVersions() match {
        case Failure(exception) =>
          sender() ! Response.Failure(exception)
        case Success(value) =>
          val list = value.map(EngineVersion(_, markedAsBroken = true))
          sender() ! Response.EngineList(list)
      }
    case Request.CreateProject(_) =>
      val _ = jvmSettings
      val _ = runner
//      runner.newProject(???, ???, ???, ???, ???, ???) // FIXME [RW] WIP
    case Request.StartLanguageServer(_) =>
      ??? // FIXME [RW] WIP
  }
}

object RuntimeVersionManagerController {
  sealed trait Request
  object Request {
    case class InstallEngine(version: SemVer, forceInstallBroken: Boolean)
        extends Request
    case class UninstallEngine(version: SemVer) extends Request
    case object ListInstalledEngines            extends Request
    case object ListAvailableEngines            extends Request
    case class CreateProject(todo: Any)         extends Request
    case class StartLanguageServer(todo: Any)   extends Request
  }

  sealed trait Response
  object Response {
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
