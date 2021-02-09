package org.enso.projectmanager.data

import nl.gn0s1s.bump.SemVer

/** Information about running language server.
  *
  * @param engineVersion the version of the started language server
  * @param sockets the sockets listened by the language server
  */
case class RunningLanguageServerInfo(
  engineVersion: SemVer,
  sockets: LanguageServerSockets
)
