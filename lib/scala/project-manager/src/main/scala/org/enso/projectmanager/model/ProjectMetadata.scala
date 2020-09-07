package org.enso.projectmanager.model

import java.time.OffsetDateTime
import java.util.UUID

/**
  * Project metadata entity.
  *
  * @param id a project id
  * @param kind a project kind
  * @param created a project creation time
  * @param lastOpened a project last open time
  */
case class ProjectMetadata(
  id: UUID,
  kind: ProjectKind,
  created: OffsetDateTime,
  lastOpened: Option[OffsetDateTime]
)

object ProjectMetadata {

  /**
    * Create an instance from the project entity.
    *
    * @param project the project entity
    * @return the project metadata
    */
  def apply(project: Project): ProjectMetadata =
    new ProjectMetadata(
      id         = project.id,
      kind       = project.kind,
      created    = project.created,
      lastOpened = project.lastOpened
    )
}
