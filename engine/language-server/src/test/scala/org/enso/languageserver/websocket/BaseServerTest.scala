package org.enso.languageserver.websocket

import java.nio.file.Files
import java.util.UUID

import akka.actor.Props
import akka.testkit.TestProbe
import org.enso.jsonrpc.{ClientControllerFactory, Protocol}
import org.enso.jsonrpc.test.JsonRpcServerTestKit
import org.enso.languageserver.{LanguageProtocol, LanguageServer}
import org.enso.languageserver.capability.CapabilityRouter
import org.enso.languageserver.data.{
  Config,
  ExecutionContextConfig,
  FileManagerConfig,
  PathWatcherConfig,
  Sha3_224VersionCalculator
}
import org.enso.languageserver.effect.ZioExec
import org.enso.languageserver.filemanager.{
  FileManager,
  FileSystem,
  ReceivesTreeUpdatesHandler
}
import org.enso.languageserver.protocol.{JsonRpc, ServerClientControllerFactory}
import org.enso.languageserver.runtime.ContextRegistry
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

  override def clientControllerFactory: ClientControllerFactory = {
    val languageServer = system.actorOf(Props(new LanguageServer(config)))
    languageServer ! LanguageProtocol.Initialize
    val zioExec = ZioExec(zio.Runtime.default)
    val fileManager =
      system.actorOf(FileManager.props(config, new FileSystem, zioExec))
    val bufferRegistry =
      system.actorOf(
        BufferRegistry.props(fileManager)(Sha3_224VersionCalculator)
      )
    val fileEventRegistry =
      system.actorOf(
        ReceivesTreeUpdatesHandler.props(config, new FileSystem, zioExec)
      )
    val contextRegistry =
      system.actorOf(
        ContextRegistry
          .props(config.executionContext, runtimeConnectorProbe.ref)
      )
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
}
