package org.enso.projectmanager.service.versionmanagement

import java.util.UUID

import org.enso.jsonrpc.Notification
import org.enso.projectmanager.data.ProgressUnit
import org.enso.projectmanager.protocol.ProjectManagementApi

/** Internal representation of progress notifications that are sent by the
  * [[ControllerInterface]].
  *
  * They are translated by the [[RequestHandler]] into protocol progress
  * notifications.
  */
sealed trait ProgressNotification
object ProgressNotification {

  /** Singals that a new task with progress has been started. */
  case class TaskStarted(
    taskId: UUID,
    total: Option[Long],
    unit: ProgressUnit
  ) extends ProgressNotification

  /** Singals an update to task's progress. */
  case class TaskUpdate(taskId: UUID, message: Option[String], done: Long)
      extends ProgressNotification

  /** Singals that a task has been finished successfully. */
  case class TaskSuccess(taskId: UUID) extends ProgressNotification

  /** Singals that a task has failed. */
  case class TaskFailure(taskId: UUID, throwable: Throwable)
      extends ProgressNotification

  /** Translates a [[ProgressNotification]] into a protocol message. */
  def translateProgressNotification(
    relatedOperationName: String,
    progressNotification: ProgressNotification
  ): Notification[_, _] = progressNotification match {
    case TaskStarted(taskId, total, unit) =>
      Notification(
        ProjectManagementApi.TaskStarted,
        ProjectManagementApi.TaskStarted.Params(
          taskId           = taskId,
          relatedOperation = relatedOperationName,
          unit             = unit,
          total            = total
        )
      )
    case TaskUpdate(taskId, message, done) =>
      Notification(
        ProjectManagementApi.TaskProgressUpdate,
        ProjectManagementApi.TaskProgressUpdate.Params(taskId, message, done)
      )
    case TaskSuccess(taskId) =>
      Notification(
        ProjectManagementApi.TaskFinished,
        ProjectManagementApi.TaskFinished.Params(taskId, None, success = true)
      )
    case TaskFailure(taskId, throwable) =>
      Notification(
        ProjectManagementApi.TaskFinished,
        ProjectManagementApi.TaskFinished
          .Params(taskId, Some(throwable.getMessage), success = false)
      )
  }
}
