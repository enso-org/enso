package org.enso.runtimeversionmanager.runner

import java.util.UUID

/** Options that are passed to the language server.
  *
  * @param rootId an id of content root
  * @param projectId an id of the project
  * @param projectCloudId an id of the cloud project
  * @param projectCloudSessionId an id of the session in cloud project
  * @param interface a interface that the server listen to
  * @param rpcPort an RPC port that the server listen to
  * @param secureRpcPort an option secure RPC port that the server listen to
  * @param dataPort a data port that the server listen to
  * @param secureDataPort an optional secure data port that the server listen to
  * @param jvm use JVM - default or provided
  */
case class LanguageServerOptions(
  rootId: UUID,
  projectId: UUID,
  projectCloudId: Option[String],
  projectCloudSessionId: Option[String],
  interface: String,
  rpcPort: Int,
  secureRpcPort: Option[Int],
  dataPort: Int,
  secureDataPort: Option[Int],
  jvm: Option[Option[java.nio.file.Path]]
)
