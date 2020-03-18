package org.enso.languageserver

import java.io.File
import java.net.URI

import akka.actor.{ActorSystem, Props}
import akka.stream.SystemMaterializer
import cats.effect.IO
import org.enso.jsonrpc.JsonRpcServer
import org.enso.languageserver.capability.CapabilityRouter
import org.enso.languageserver.data.{
  Config,
  ContentBasedVersioning,
  Sha3_224VersionCalculator
}
import org.enso.languageserver.filemanager.{FileSystem, FileSystemApi}
import org.enso.languageserver.protocol.{JsonRpc, ServerClientControllerFactory}
import org.enso.languageserver.runtime.RuntimeConnector
import org.enso.languageserver.text.BufferRegistry
import org.enso.polyglot.{LanguageInfo, RuntimeServerInfo}
import org.graalvm.polyglot.Context
import org.graalvm.polyglot.io.MessageEndpoint

/**
  * A main module containing all components of th server.
  *
  * @param serverConfig a server config
  */
class MainModule(serverConfig: LanguageServerConfig) {

  lazy val languageServerConfig = Config(
    Map(serverConfig.contentRootUuid -> new File(serverConfig.contentRootPath))
  )

  lazy val fileSystem: FileSystemApi[IO] = new FileSystem[IO]

  implicit val versionCalculator: ContentBasedVersioning =
    Sha3_224VersionCalculator

  implicit val system = ActorSystem()

  implicit val materializer = SystemMaterializer.get(system)

  lazy val languageServer =
    system.actorOf(
      Props(new LanguageServer(languageServerConfig, fileSystem)),
      "server"
    )

  lazy val bufferRegistry =
    system.actorOf(BufferRegistry.props(languageServer), "buffer-registry")

  lazy val capabilityRouter =
    system.actorOf(CapabilityRouter.props(bufferRegistry), "capability-router")

  lazy val runtimeConnector =
    system.actorOf(RuntimeConnector.props, "runtime-connector")

  val context = Context
    .newBuilder(LanguageInfo.ID)
    .allowAllAccess(true)
    .allowExperimentalOptions(true)
    .option(RuntimeServerInfo.ENABLE_OPTION, "true")
    .serverTransport((uri: URI, peerEndpoint: MessageEndpoint) => {
      if (uri.toString == RuntimeServerInfo.URI) {
        val connection = new RuntimeConnector.Endpoint(
          runtimeConnector,
          peerEndpoint
        )
        runtimeConnector ! RuntimeConnector.Initialize(peerEndpoint)
        connection
      } else null
    })
    .build()

  lazy val clientControllerFactory = new ServerClientControllerFactory(
    languageServer,
    bufferRegistry,
    capabilityRouter
  )

  lazy val server =
    new JsonRpcServer(JsonRpc.protocol, clientControllerFactory)

}
