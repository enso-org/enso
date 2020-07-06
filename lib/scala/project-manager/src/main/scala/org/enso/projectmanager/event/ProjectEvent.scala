package org.enso.projectmanager.event

import java.util.UUID

trait ProjectEvent extends Event

object ProjectEvent {

  case class ProjectClosed(projectId: UUID) extends ProjectEvent

}
