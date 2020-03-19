package org.enso.languageserver.websocket

import java.nio.file.Files
import java.util.UUID

import akka.actor.Props
import org.enso.jsonrpc.{ClientControllerFactory, Protocol}
import org.enso.jsonrpc.test.JsonRpcServerTestKit
import org.enso.languageserver.{LanguageProtocol, LanguageServer}
import org.enso.languageserver.effect.ZioExec
import org.enso.languageserver.capability.CapabilityRouter
import org.enso.languageserver.data.{
  Config,
  FileManagerConfig,
  Sha3_224VersionCalculator
}
import org.enso.languageserver.filemanager.{FileManager, FileSystem}
import org.enso.languageserver.protocol.{JsonRpc, ServerClientControllerFactory}
import org.enso.languageserver.text.BufferRegistry

import scala.concurrent.duration._

class BaseServerTest extends JsonRpcServerTestKit {

  val testContentRoot   = Files.createTempDirectory(null)
  val testContentRootId = UUID.randomUUID()
  val config = Config(
    Map(testContentRootId -> testContentRoot.toFile),
    FileManagerConfig(timeout = 3.seconds)
  )

  testContentRoot.toFile.deleteOnExit()

  override def protocol: Protocol = JsonRpc.protocol

  override def clientControllerFactory: ClientControllerFactory = {
    val zioExec        = ZioExec(zio.Runtime.default)
    val languageServer = system.actorOf(Props(new LanguageServer(config)))
    languageServer ! LanguageProtocol.Initialize
    val fileManager =
      system.actorOf(FileManager.props(config, new FileSystem, zioExec))
    val bufferRegistry =
      system.actorOf(
        BufferRegistry.props(fileManager)(Sha3_224VersionCalculator)
      )
    lazy val capabilityRouter =
      system.actorOf(CapabilityRouter.props(bufferRegistry))

    new ServerClientControllerFactory(
      languageServer,
      bufferRegistry,
      capabilityRouter,
      fileManager
    )
  }

}
