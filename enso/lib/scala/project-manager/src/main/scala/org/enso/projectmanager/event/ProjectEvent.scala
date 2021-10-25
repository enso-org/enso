package org.enso.projectmanager.event

import java.util.UUID

/** Base trait for all project events.
  */
trait ProjectEvent extends Event

object ProjectEvent {

  /** Notifies that project has been closed.
    *
    * @param projectId the id of the project that was closed
    */
  case class ProjectClosed(projectId: UUID) extends ProjectEvent

}
