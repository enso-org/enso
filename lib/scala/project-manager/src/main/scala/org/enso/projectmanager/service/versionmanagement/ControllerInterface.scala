package org.enso.projectmanager.service.versionmanagement

import java.util.UUID

import akka.actor.ActorRef
import nl.gn0s1s.bump.SemVer
import org.enso.cli.task.{ProgressListener, TaskProgress}
import org.enso.projectmanager.data.ProgressUnit
import org.enso.runtimeversionmanager.components.{
  GraalVMVersion,
  RuntimeVersionManagementUserInterface
}
import org.enso.runtimeversionmanager.locking.Resource

import scala.util.{Failure, Success, Try}

class ControllerInterface(
  progressTracker: ActorRef,
  allowMissingComponents: Boolean,
  allowBrokenComponents: Boolean
) extends RuntimeVersionManagementUserInterface {
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

  override def shouldInstallMissingEngine(version: SemVer): Boolean =
    allowMissingComponents

  override def shouldInstallMissingRuntime(version: GraalVMVersion): Boolean =
    allowMissingComponents

  override def shouldInstallBrokenEngine(version: SemVer): Boolean =
    allowBrokenComponents

  override def logInfo(message: => String): Unit = ()

  private val waitingForResources =
    collection.concurrent.TrieMap[String, UUID]()

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

  override def finishWaitingForResource(resource: Resource): Unit = {
    for (uuid <- waitingForResources.remove(resource.name)) {
      progressTracker ! ProgressNotification.TaskSuccess(uuid)
    }
  }
}
