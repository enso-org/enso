package org.enso.languageserver.websocket

import java.nio.file.Files
import java.util.UUID

import akka.actor.Props
import cats.effect.IO
import org.enso.jsonrpc.{ClientControllerFactory, Protocol}
import org.enso.jsonrpc.test.JsonRpcServerTestKit
import org.enso.languageserver.{LanguageProtocol, LanguageServer}
import org.enso.languageserver.capability.CapabilityRouter
import org.enso.languageserver.data.{Config, Sha3_224VersionCalculator}
import org.enso.languageserver.filemanager.FileSystem
import org.enso.languageserver.protocol.{JsonRpc, ServerClientControllerFactory}
import org.enso.languageserver.text.BufferRegistry

class BaseServerTest extends JsonRpcServerTestKit {

  val testContentRoot   = Files.createTempDirectory(null)
  val testContentRootId = UUID.randomUUID()
  val config            = Config(Map(testContentRootId -> testContentRoot.toFile))

  testContentRoot.toFile.deleteOnExit()

  override def protocol: Protocol = JsonRpc.protocol

  override def clientControllerFactory: ClientControllerFactory = {
    val languageServer =
      system.actorOf(
        Props(new LanguageServer(config, new FileSystem[IO]))
      )
    languageServer ! LanguageProtocol.Initialize
    val bufferRegistry =
      system.actorOf(
        BufferRegistry.props(languageServer)(Sha3_224VersionCalculator)
      )

    lazy val capabilityRouter =
      system.actorOf(CapabilityRouter.props(bufferRegistry))

    new ServerClientControllerFactory(
      languageServer,
      bufferRegistry,
      capabilityRouter
    )
  }

}
