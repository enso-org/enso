package org.enso.cli.task.notifications

import org.enso.jsonrpc.Notification

import java.util.UUID

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
    unit: SerializableProgressUnit
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
        TaskNotificationApi.TaskStarted,
        TaskNotificationApi.TaskStarted.Params(
          taskId           = taskId,
          relatedOperation = relatedOperationName,
          unit             = unit,
          total            = total
        )
      )
    case TaskUpdate(taskId, message, done) =>
      Notification(
        TaskNotificationApi.TaskProgressUpdate,
        TaskNotificationApi.TaskProgressUpdate.Params(taskId, message, done)
      )
    case TaskSuccess(taskId) =>
      Notification(
        TaskNotificationApi.TaskFinished,
        TaskNotificationApi.TaskFinished.Params(taskId, None, success = true)
      )
    case TaskFailure(taskId, throwable) =>
      Notification(
        TaskNotificationApi.TaskFinished,
        TaskNotificationApi.TaskFinished
          .Params(taskId, Some(throwable.getMessage), success = false)
      )
  }
}
