package org.enso.languageserver.requesthandler.file

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.util.UnhandledLogging

import scala.annotation.unused
import scala.concurrent.duration.FiniteDuration

// TODO [AA] Doc, remove @unused
class WriteBytesHandler(
  @unused requestTimeout: FiniteDuration,
  @unused fileManager: ActorRef,
  @unused replyTo: ActorRef
) extends Actor
    with LazyLogging
    with UnhandledLogging {
//  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    ???
  }
}
object WriteBytesHandler {

  /** Creates a configuration object used to create a [[WriteBytesHandler]].
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
    Props(new WriteBytesHandler(timeout, fileManager, replyTo))
  }
}
