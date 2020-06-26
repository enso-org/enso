package org.enso.languageserver.websocket.json

import java.nio.file.Files
import java.util.UUID

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
import org.enso.languageserver.io.{
  InputRedirectionController,
  ObservableOutputStream,
  ObservablePipedInputStream,
  OutputKind,
  OutputRedirectionController
}
import org.enso.languageserver.protocol.json.{
  JsonConnectionControllerFactory,
  JsonRpc
}
import org.enso.languageserver.runtime.{
  ContextRegistry,
  SuggestionsDatabaseEventsListener
}
import org.enso.languageserver.session.SessionRouter
import org.enso.languageserver.text.BufferRegistry

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

  val stdOut    = new ObservableOutputStream
  val stdErr    = new ObservableOutputStream
  val stdInSink = new ObservableOutputStream
  val stdIn     = new ObservablePipedInputStream(stdInSink)

  val sessionRouter =
    system.actorOf(SessionRouter.props())

  val stdOutController =
    system.actorOf(
      OutputRedirectionController
        .props(stdOut, OutputKind.StandardOutput, sessionRouter)
    )

  val stdErrController =
    system.actorOf(
      OutputRedirectionController
        .props(stdErr, OutputKind.StandardError, sessionRouter)
    )

  val stdInController =
    system.actorOf(
      InputRedirectionController.props(stdIn, stdInSink, sessionRouter)
    )

  override def clientControllerFactory: ClientControllerFactory = {
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
      system.actorOf(
        ContextRegistry.props(config, runtimeConnectorProbe.ref, sessionRouter)
      )

    val suggestionsDatabaseEventsListener =
      system.actorOf(SuggestionsDatabaseEventsListener.props(sessionRouter))

    val capabilityRouter =
      system.actorOf(
        CapabilityRouter.props(
          bufferRegistry,
          fileEventRegistry,
          suggestionsDatabaseEventsListener
        )
      )

    new JsonConnectionControllerFactory(
      bufferRegistry,
      capabilityRouter,
      fileManager,
      contextRegistry,
      stdOutController,
      stdErrController,
      stdInController
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
