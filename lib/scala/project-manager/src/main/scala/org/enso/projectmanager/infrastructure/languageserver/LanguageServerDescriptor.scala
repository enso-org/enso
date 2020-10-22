package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

import org.enso.projectmanager.boot.configuration.NetworkConfig

/** A descriptor used to start up a Language Server.
  *
  * @param name a name of the LS
  * @param rootId a content root id
  * @param root a path to the content root
  * @param networkConfig a network config
  */
case class LanguageServerDescriptor(
  name: String,
  rootId: UUID,
  root: String,
  networkConfig: NetworkConfig
)
