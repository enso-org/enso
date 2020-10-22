package org.enso.projectmanager.data

import java.time.OffsetDateTime
import java.util.UUID

/** Contains project metadata.
  *
  * @param name the name of the project
  * @param id the project id
  * @param lastOpened the last opened datetime
  */
case class ProjectMetadata(
  name: String,
  id: UUID,
  lastOpened: Option[OffsetDateTime]
)
