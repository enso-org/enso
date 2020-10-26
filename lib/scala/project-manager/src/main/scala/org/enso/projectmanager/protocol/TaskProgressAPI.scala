package org.enso.projectmanager.protocol

import java.util.UUID

import org.enso.jsonrpc.{HasParams, Method}

object TaskProgressAPI {
  case object TaskStarted extends Method("task/started") {

    case class Params(
      taskId: UUID,
      relatedOperation: String,
      unit: String,
      total: Option[Int]
    )

    implicit val hasParams = new HasParams[this.type] {
      type Params = TaskStarted.Params
    }
  }

  case object TaskProgressUpdate extends Method("task/progress-update") {

    case class Params(
      taskId: UUID,
      message: Option[String],
      done: Int
    )

    implicit val hasParams = new HasParams[this.type] {
      type Params = TaskProgressUpdate.Params
    }
  }

  case object TaskFinished extends Method("task/progress-update") {

    case class Params(
      taskId: UUID,
      message: Option[String],
      success: Boolean
    )

    implicit val hasParams = new HasParams[this.type] {
      type Params = TaskFinished.Params
    }
  }
}
