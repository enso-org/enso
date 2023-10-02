package org.enso.projectmanager.data

import java.time.OffsetDateTime
import java.util.UUID

/** Contains project metadata.
  *
  * @param name the name of the project
  * @param namespace the namespace of the project
  * @param id the project id
  * @param created the project creation time
  * @param lastOpened the last opened datetime
  */
case class ProjectMetadata(
  name: String,
  namespace: String,
  id: UUID,
  created: OffsetDateTime,
  lastOpened: Option[OffsetDateTime]
)
