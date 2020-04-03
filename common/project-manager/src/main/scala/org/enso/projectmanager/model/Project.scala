package org.enso.projectmanager.model

import java.time.OffsetDateTime
import java.util.UUID

/**
  * Project entity.
  *
  * @param id a project id
  * @param name a project name
  * @param kind a project kind
  * @param created a project creation time
  * @param lastOpened a project last open time
  * @param path a path to the project structure
  */
case class Project(
  id: UUID,
  name: String,
  kind: ProjectKind,
  created: OffsetDateTime,
  lastOpened: Option[OffsetDateTime] = None,
  path: Option[String]               = None
)
