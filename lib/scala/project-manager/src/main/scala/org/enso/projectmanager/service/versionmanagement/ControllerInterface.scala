package org.enso.projectmanager.service.versionmanagement

import java.util.UUID

import akka.actor.ActorRef
import com.typesafe.scalalogging.Logger
import nl.gn0s1s.bump.SemVer
import org.enso.cli.task.{ProgressListener, TaskProgress}
import org.enso.projectmanager.data.ProgressUnit
import org.enso.runtimeversionmanager.components.{
  GraalVMVersion,
  RuntimeVersionManagementUserInterface
}
import org.enso.runtimeversionmanager.locking.Resource

import scala.util.{Failure, Success, Try}

/** A [[RuntimeVersionManagementUserInterface]] that sends
  * [[ProgressNotification]] to the specified actors (both for usual tasks and
  * indeterminate progress when waiting on locks).
  *
  * @param progressTracker the actor to send progress updates to
  * @param allowMissingComponents specifies if missing components should be
  *                               automatically installed
  * @param allowBrokenComponents specifies if broken components can be installed
  */
class ControllerInterface(
  progressTracker: ActorRef,
  allowMissingComponents: Boolean,
  allowBrokenComponents: Boolean
) extends RuntimeVersionManagementUserInterface {

  /** @inheritdoc */
  override def trackProgress(message: String, task: TaskProgress[_]): Unit = {
    var uuid: Option[UUID] = None

    /** Initializes the task on first invocation and just returns the
      * generated UUID on further invocations.
      */
    def initializeTask(total: Option[Long]): UUID = uuid match {
      case Some(value) => value
      case None =>
        val generated = UUID.randomUUID()
        uuid = Some(generated)
        val unit = ProgressUnit.fromTask(task)
        progressTracker ! ProgressNotification.TaskStarted(
          generated,
          total,
          unit
        )
        generated
    }
    task.addProgressListener(new ProgressListener[Any] {

      /** @inheritdoc */
      override def progressUpdate(
        done: Long,
        total: Option[Long]
      ): Unit = {
        val uuid = initializeTask(total)
        progressTracker ! ProgressNotification.TaskUpdate(
          uuid,
          Some(message),
          done
        )
      }

      /** @inheritdoc */
      override def done(result: Try[Any]): Unit = result match {
        case Failure(exception) =>
          val uuid = initializeTask(None)
          progressTracker ! ProgressNotification.TaskFailure(uuid, exception)
        case Success(_) =>
          val uuid = initializeTask(None)
          progressTracker ! ProgressNotification.TaskSuccess(uuid)
      }
    })
  }

  /** @inheritdoc */
  override def shouldInstallMissingEngine(version: SemVer): Boolean =
    allowMissingComponents

  /** @inheritdoc */
  override def shouldInstallMissingRuntime(version: GraalVMVersion): Boolean =
    allowMissingComponents

  /** @inheritdoc */
  override def shouldInstallBrokenEngine(version: SemVer): Boolean =
    allowBrokenComponents

  /** @inheritdoc */
  override def logInfo(message: => String): Unit =
    Logger[ControllerInterface].info(message)

  private val waitingForResources =
    collection.concurrent.TrieMap[String, UUID]()

  /** @inheritdoc */
  override def startWaitingForResource(resource: Resource): Unit = {
    val uuid = UUID.randomUUID()
    progressTracker ! ProgressNotification.TaskStarted(
      uuid,
      None,
      ProgressUnit.Other
    )
    progressTracker ! ProgressNotification.TaskUpdate(
      uuid,
      Some(resource.waitMessage),
      0
    )
    waitingForResources.put(resource.name, uuid)
  }

  /** @inheritdoc */
  override def finishWaitingForResource(resource: Resource): Unit = {
    for (uuid <- waitingForResources.remove(resource.name)) {
      progressTracker ! ProgressNotification.TaskSuccess(uuid)
    }
  }
}
