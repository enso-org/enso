package org.enso.projectmanager.model

import java.time.OffsetDateTime
import java.util.UUID

import org.enso.pkg.{DefaultEnsoVersion, EnsoVersion}

/** Project entity.
  *
  * @param id a project id
  * @param name a project name
  * @param kind a project kind
  * @param created a project creation time
  * @param engineVersion version of the engine associated with the project
  * @param lastOpened a project last open time
  * @param path a path to the project structure
  */
case class Project(
  id: UUID,
  name: String,
  kind: ProjectKind,
  created: OffsetDateTime,
  engineVersion: EnsoVersion         = DefaultEnsoVersion,
  lastOpened: Option[OffsetDateTime] = None,
  path: Option[String]               = None
)
