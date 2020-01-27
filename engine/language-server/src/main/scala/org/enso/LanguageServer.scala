package org.enso

import akka.actor.{Actor, ActorLogging, Props}
import org.enso.languageserver.{
  NotificationReceived,
  Notifications,
  RequestReceived,
  Requests
}
import org.enso.polyglot.ExecutionContext

/** The Language Server component of Enso Engine.
  *
  * Wraps the runtime itself, and uses the APIs provided by the interpreter
  * and the compiler to service the requests sent to the Enso Engine.
  *
  * @param context Polyglot Execution context.
  */
class LanguageServer(context: ExecutionContext)
    extends Actor
    with ActorLogging {
  override def receive: Receive = {
    case Requests.Initialize(id, actorRef) =>
      val msg = "LanguageServer: Initialize received"
      log.info(msg)
      sender() ! RequestReceived.Initialize(id, actorRef)

    case Requests.Shutdown(id, actorRef) =>
      val msg = "LanguageServer: Shutdown received"
      log.info(msg)
      sender() ! RequestReceived.Shutdown(id, actorRef)

    case Notifications.Initialized =>
      val msg = "LanguageServer: Initialized received"
      log.info(msg)
      sender() ! NotificationReceived.Initialized

    case Notifications.Exit =>
      val msg = "LanguageServer: Exit received"
      log.info(msg)
      sender() ! NotificationReceived.Exit

    case requestOrNotification =>
      val msg = "LanguageServer: unexpected request or notification " +
        requestOrNotification
      log.error(msg)
  }
}
object LanguageServer {
  def props(context: ExecutionContext): Props =
    Props(new LanguageServer(context))
}
