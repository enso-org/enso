package org.enso

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.polyglot.ExecutionContext

/** The language server component wraps the runtime itself, and uses the APIs
  * provided by the interpreter and the compiler to service the requests sent
  * to the Enso Engine.
  */
class LanguageServer(context: ExecutionContext)
    extends Actor
    with ActorLogging {
  override def receive: Receive = {
    case LanguageServer.Initialize(id, actorRef) =>
      val msg = "LanguageServer: Initialize received"
      log.info(msg)
      sender() ! LanguageServer.InitializeReceived(id, actorRef)

    case LanguageServer.Initialized =>
      val msg = "LanguageServer: Initialized received"
      log.info(msg)
      sender() ! LanguageServer.InitializedReceived
  }
}
object LanguageServer {

  /** Akka message sent by Gateway received LSP request `initialize`. */
  case class Initialize(id: Int, replyTo: ActorRef)

  /** Akka message sent by Gateway received LSP notification `initialized`. */
  case object Initialized

  /** Language server response to [[Initialize]]. */
  case class InitializeReceived(id: Int, replyTo: ActorRef)

  /** Language server response to [[Initialized]]. */
  case object InitializedReceived

  def props(context: ExecutionContext): Props =
    Props(new LanguageServer(context))
}
