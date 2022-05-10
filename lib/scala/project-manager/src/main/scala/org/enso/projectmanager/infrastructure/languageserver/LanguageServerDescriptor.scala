package org.enso.projectmanager.infrastructure.languageserver

import java.util.UUID

import akka.http.scaladsl.model.Uri
import nl.gn0s1s.bump.SemVer
import org.enso.projectmanager.boot.configuration.NetworkConfig
import org.enso.projectmanager.versionmanagement.DistributionConfiguration
import org.enso.runtimeversionmanager.runner.JVMSettings

import scala.concurrent.Future

/** A descriptor specifying options related to starting a Language Server.
  *
  * @param name a name of the LS
  * @param rootId a content root id
  * @param rootPath a path to the content root
  * @param networkConfig a network config
  * @param distributionConfiguration configuration of current distribution, used
  *                                  to find installed (or install new) engine
  *                                  versions
  * @param engineVersion version of the langauge server's engine to use
  * @param jvmSettings settings to use for the JVM that will host the engine
  * @param discardOutput specifies if the process output should be discarded or
  *                      printed to parent's streams
  * @param profilingEnabled enables the language server profiling
  * @param deferredLoggingServiceEndpoint a future that is completed once the
  *                                       logging service has been fully set-up;
  *                                       if the child component should connect
  *                                       to the logging service, it should
  *                                       contain the Uri to connect to
  */
case class LanguageServerDescriptor(
  name: String,
  rootId: UUID,
  rootPath: String,
  networkConfig: NetworkConfig,
  distributionConfiguration: DistributionConfiguration,
  engineVersion: SemVer,
  jvmSettings: JVMSettings,
  discardOutput: Boolean,
  profilingEnabled: Boolean,
  deferredLoggingServiceEndpoint: Future[Option[Uri]]
)
