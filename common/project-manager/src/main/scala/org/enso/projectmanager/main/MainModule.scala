package org.enso.projectmanager.main

import akka.actor.ActorSystem
import akka.stream.SystemMaterializer
import org.enso.jsonrpc.JsonRpcServer
import org.enso.projectmanager.main.configuration.ProjectManagerConfig
import org.enso.projectmanager.protocol.{
  JsonRpc,
  ManagerClientControllerFactory
}

/**
  * A main module containing all components of the project manager.
  *
  * @param config a server config
  */
class MainModule(config: ProjectManagerConfig) {

  implicit val system = ActorSystem()

  implicit val materializer = SystemMaterializer.get(system)

  lazy val clientControllerFactory = new ManagerClientControllerFactory(system)

  lazy val server = new JsonRpcServer(JsonRpc.protocol, clientControllerFactory)

}
