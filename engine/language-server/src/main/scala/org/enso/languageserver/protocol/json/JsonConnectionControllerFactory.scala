package org.enso.languageserver.protocol.json

import akka.actor.{ActorRef, ActorSystem}
import org.enso.jsonrpc.ClientControllerFactory
import org.enso.languageserver.boot.resource.InitializationComponent
import org.enso.languageserver.data.Config
import org.enso.languageserver.libraries.LibraryConfig

import java.util.UUID

/** Language server client controller factory.
  *
  * @param mainComponent the main initialization logic
  * @param bufferRegistry the buffer registry actor ref
  * @param capabilityRouter the capability router actor ref
  * @param fileManager performs operations with file system
  * @param vcsManager performs operations with VCS
  * @param contentRootManager manages the available content roots
  * @param contextRegistry a router that dispatches execution context requests
  * @param suggestionsHandler a reference to the suggestions requests handler
  * @param runtimeConnector a reference to the runtime connector
  * @param idlenessMonitor a reference to the idleness monitor actor
  * @param projectSettingsManager a reference to the project settings manager
  * @param profilingManager a reference to the profiling manager
  * @param libraryConfig configuration of the library ecosystem
  * @param system the actor system
  */
class JsonConnectionControllerFactory(
  mainComponent: InitializationComponent,
  bufferRegistry: ActorRef,
  capabilityRouter: ActorRef,
  fileManager: ActorRef,
  vcsManager: ActorRef,
  contentRootManager: ActorRef,
  contextRegistry: ActorRef,
  suggestionsHandler: ActorRef,
  stdOutController: ActorRef,
  stdErrController: ActorRef,
  stdInController: ActorRef,
  runtimeConnector: ActorRef,
  idlenessMonitor: ActorRef,
  projectSettingsManager: ActorRef,
  profilingManager: ActorRef,
  libraryConfig: LibraryConfig,
  config: Config
)(implicit system: ActorSystem)
    extends ClientControllerFactory {

  /** Creates a client controller actor.
    *
    * @param clientId the internal client id.
    * @return the client controller actor
    */
  override def createClientController(clientId: UUID): ActorRef =
    system.actorOf(
      JsonConnectionController.props(
        connectionId           = clientId,
        mainComponent          = mainComponent,
        bufferRegistry         = bufferRegistry,
        capabilityRouter       = capabilityRouter,
        fileManager            = fileManager,
        vcsManager             = vcsManager,
        contentRootManager     = contentRootManager,
        contextRegistry        = contextRegistry,
        suggestionsHandler     = suggestionsHandler,
        stdOutController       = stdOutController,
        stdErrController       = stdErrController,
        stdInController        = stdInController,
        runtimeConnector       = runtimeConnector,
        idlenessMonitor        = idlenessMonitor,
        projectSettingsManager = projectSettingsManager,
        profilingManager       = profilingManager,
        libraryConfig          = libraryConfig,
        languageServerConfig   = config
      ),
      s"json-connection-controller-$clientId"
    )
}
