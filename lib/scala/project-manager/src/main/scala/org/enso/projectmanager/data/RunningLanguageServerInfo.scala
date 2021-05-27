package org.enso.projectmanager.data

import nl.gn0s1s.bump.SemVer

/** Information about running language server.
  *
  * @param engineVersion the version of the started language server
  * @param sockets the sockets listened by the language server
  * @param projectName the name of the project
  */
case class RunningLanguageServerInfo(
  engineVersion: SemVer,
  sockets: LanguageServerSockets,
  projectName: String
)
