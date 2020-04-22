package org.enso.languageserver.websocket

import java.nio.file.Files
import java.util.UUID

import akka.actor.Props
import akka.testkit.TestProbe
import io.circe.literal._
import org.enso.jsonrpc.test.JsonRpcServerTestKit
import org.enso.jsonrpc.{ClientControllerFactory, Protocol}
import org.enso.languageserver.capability.CapabilityRouter
import org.enso.languageserver.data._
import org.enso.languageserver.effect.ZioExec
import org.enso.languageserver.filemanager.{
  FileManager,
  FileSystem,
  ReceivesTreeUpdatesHandler
}
import org.enso.languageserver.protocol.{JsonRpc, ServerClientControllerFactory}
import org.enso.languageserver.runtime.ContextRegistry
import org.enso.languageserver.text.BufferRegistry
import org.enso.languageserver.{LanguageProtocol, LanguageServer}

import scala.concurrent.duration._

class BaseServerTest extends JsonRpcServerTestKit {

  val testContentRoot   = Files.createTempDirectory(null).toRealPath()
  val testContentRootId = UUID.randomUUID()
  val config = Config(
    Map(testContentRootId -> testContentRoot.toFile),
    FileManagerConfig(timeout = 3.seconds),
    PathWatcherConfig(),
    ExecutionContextConfig(requestTimeout = 3.seconds)
  )
  val runtimeConnectorProbe = TestProbe()

  testContentRoot.toFile.deleteOnExit()

  override def protocol: Protocol = JsonRpc.protocol

  override def clientControllerFactory: ClientControllerFactory = {
    val languageServer = system.actorOf(Props(new LanguageServer(config)))
    languageServer ! LanguageProtocol.Initialize
    val zioExec = ZioExec(zio.Runtime.default)
    val fileManager =
      system.actorOf(FileManager.props(config, new FileSystem, zioExec))
    val bufferRegistry =
      system.actorOf(
        BufferRegistry.props(fileManager, runtimeConnectorProbe.ref)(
          Sha3_224VersionCalculator
        )
      )
    val fileEventRegistry =
      system.actorOf(
        ReceivesTreeUpdatesHandler.props(config, new FileSystem, zioExec)
      )
    val contextRegistry =
      system.actorOf(ContextRegistry.props(config, runtimeConnectorProbe.ref))
    lazy val capabilityRouter =
      system.actorOf(CapabilityRouter.props(bufferRegistry, fileEventRegistry))

    new ServerClientControllerFactory(
      languageServer,
      bufferRegistry,
      capabilityRouter,
      fileManager,
      contextRegistry
    )
  }

  def getInitialisedWsClient(): WsTestClient = {
    val client = new WsTestClient(address)
    initSession(client)
    client
  }

  private def initSession(client: WsTestClient): UUID = {
    val clientId = UUID.randomUUID()
    client.send(json"""
          { "jsonrpc": "2.0",
            "method": "session/initProtocolConnection",
            "id": 1,
            "params": {
              "clientId": $clientId
            }
          }
          """)
    client.expectJson(json"""
            { "jsonrpc":"2.0",
              "id":1,
              "result":{
                "contentRoots":[ $testContentRootId ]
              }
            }
              """)
    clientId
  }

}
