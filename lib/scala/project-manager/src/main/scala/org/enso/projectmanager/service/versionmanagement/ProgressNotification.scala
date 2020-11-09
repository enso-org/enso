package org.enso.projectmanager.service.versionmanagement

import java.util.UUID

import org.enso.projectmanager.data.ProgressUnit

sealed trait ProgressNotification
object ProgressNotification {
  case class TaskStarted(
    taskId: UUID,
    total: Option[Long],
    unit: ProgressUnit
  ) extends ProgressNotification
  case class TaskUpdate(taskId: UUID, message: Option[String], done: Long)
      extends ProgressNotification
  case class TaskSuccess(taskId: UUID) extends ProgressNotification
  case class TaskFailure(taskId: UUID, throwable: Throwable)
      extends ProgressNotification
}
