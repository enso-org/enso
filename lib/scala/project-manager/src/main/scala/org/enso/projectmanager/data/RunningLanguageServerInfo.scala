package org.enso.projectmanager.data

import org.enso.semver.SemVer

/** Information about running language server.
  *
  * @param engineVersion the version of the started language server
  * @param sockets the sockets listened by the language server
  * @param projectName the name of the project
  * @param projectNormalizedName the normalized name of the project
  * @param projectNamespace the namespace of the project
  */
case class RunningLanguageServerInfo(
  engineVersion: SemVer,
  sockets: LanguageServerSockets,
  projectName: String,
  projectNormalizedName: String,
  projectNamespace: String
)
