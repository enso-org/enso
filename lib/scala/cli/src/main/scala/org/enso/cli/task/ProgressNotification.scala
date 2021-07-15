package org.enso.cli.task

import java.util.UUID

/** Internal representation of progress notifications. */
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
}
