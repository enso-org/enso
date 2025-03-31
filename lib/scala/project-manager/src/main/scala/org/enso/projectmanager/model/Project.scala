package org.enso.projectmanager.model

import org.enso.editions.Editions

import java.io.File
import java.nio.file.attribute.FileTime
import java.time.OffsetDateTime
import java.util.UUID

/** Project entity.
  *
  * @param id a project id
  * @param name a project name
  * @param module a module name
  * @param namespace a project namespace
  * @param kind a project kind
  * @param created a project creation time
  * @param edition the edition configuration associated with the project
  * @param jvmModeEnabled should the JVM mode be enabled for the project
  * @param path a path to the project structure
  * @param lastOpened a project last open time
  * @param directoryCreationTime a project's directory creation time
  */
case class Project(
  id: UUID,
  name: String,
  module: String,
  namespace: String,
  kind: ProjectKinds.ProjectKind,
  created: OffsetDateTime,
  edition: Option[Editions.RawEdition],
  jvmModeEnabled: Option[Boolean],
  path: File,
  lastOpened: Option[OffsetDateTime]      = None,
  directoryCreationTime: Option[FileTime] = None
) {

  def isJvmModeEnabled(): Boolean = jvmModeEnabled.getOrElse(false)
}
