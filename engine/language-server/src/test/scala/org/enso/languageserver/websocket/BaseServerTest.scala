package org.enso.languageserver.websocket

import java.nio.file.Files
import java.util.UUID

import akka.actor.{ActorRef, Props}
import org.enso.jsonrpc.{ClientControllerFactory, Protocol}
import org.enso.jsonrpc.test.JsonRpcServerTestKit
import org.enso.languageserver.{LanguageProtocol, LanguageServer}
import org.enso.languageserver.effect.ZioExec
import org.enso.languageserver.filemanager.{FileManagerProtocol, Path}
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
    val languageServer = system.actorOf(Props(new LanguageServer(config)))
    languageServer ! LanguageProtocol.Initialize
    val fileManager = getFileManager()
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

  private def getFileManager(): ActorRef = {
    val zioExec = ZioExec(zio.Runtime.default)
    val fileManager =
      system.actorOf(FileManager.props(config, new FileSystem, zioExec))
    // Tests requiring FileManager can randomly fail with timeout on
    // Windows. And it's always the first one that fails. I assume it happens
    // due to a cold Zio executor. Here we send a few messages to warm up the
    // FileManager.
    fileManager ! FileManagerProtocol.ExistsFile(
      Path(testContentRootId, Vector())
    )
    fileManager
  }
}
