package org.enso.projectmanager.model

import java.time.OffsetDateTime
import java.util.UUID

case class ProjectMetadata(
  id: UUID,
  kind: ProjectKind,
  created: OffsetDateTime,
  lastOpened: Option[OffsetDateTime]
)

object ProjectMetadata {

  def apply(project: Project): ProjectMetadata =
    new ProjectMetadata(
      id         = project.id,
      kind       = project.kind,
      created    = project.created,
      lastOpened = project.lastOpened
    )
}
