package org.enso.projectmanager.data

import java.time.OffsetDateTime
import java.util.UUID

import org.enso.pkg.EnsoVersion

/** Contains project metadata.
  *
  * @param name the name of the project
  * @param id the project id
  * @param engineVersion version of the engine associated with the project
  * @param lastOpened the last opened datetime
  */
case class ProjectMetadata(
  name: String,
  id: UUID,
  engineVersion: EnsoVersion,
  lastOpened: Option[OffsetDateTime]
)
