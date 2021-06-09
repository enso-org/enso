package org.enso.languageserver.requesthandler.file

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.protocol.binary.factory.ErrorFactory
import org.enso.languageserver.protocol.binary.{ChecksumBytesCommand, InboundMessage}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.languageserver.util.file.PathUtils

import scala.annotation.unused
import scala.concurrent.duration.FiniteDuration

// TODO [AA] Doc, remove @unused
class ChecksumBytesHandler(
  @unused requestTimeout: FiniteDuration,
  @unused fileManager: ActorRef,
  @unused replyTo: ActorRef
) extends Actor
    with LazyLogging
    with UnhandledLogging {
  override def receive: Receive = requestStage

  private def requestStage: Receive = { case msg: InboundMessage =>
    val payload = msg.payload(new ChecksumBytesCommand).asInstanceOf[ChecksumBytesCommand]
    val segment = payload.segment
    val path = PathUtils.convertBinaryPath(segment.path)
    println(path)
    replyTo ! ErrorFactory.createGenericError(1001, "AAAAAAAAA")
  }
}
object ChecksumBytesHandler {

  /** Creates a configuration object used to create a [[ChecksumBytesHandler]].
    *
    * @param timeout the request timeout
    * @param fileManager the file system manager actor
    * @param replyTo the outbound channel delivering replies to the client
    * @return a configuration object
    */
  def props(
    timeout: FiniteDuration,
    fileManager: ActorRef,
    replyTo: ActorRef
  ): Props = {
    Props(new ChecksumBytesHandler(timeout, fileManager, replyTo))
  }
}
