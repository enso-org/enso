package org.enso.jsonrpc

import akka.actor.{
  Actor,
  ActorRef,
  OneForOneStrategy,
  Props,
  Stash,
  SupervisorStrategy
}
import com.typesafe.scalalogging.LazyLogging

import java.util.UUID

/** An actor responsible for supervising the [[MessageHandler]].
  *
  * @param protocol a protocol supported be the server
  * @param clientControllerFactory a factory used to create a client controller
  */
final class MessageHandlerSupervisor(
  clientControllerFactory: ClientControllerFactory,
  protocol: Protocol
) extends Actor
    with LazyLogging
    with Stash {

  import MessageHandlerSupervisor._

  override def preStart(): Unit = {
    self ! Initialize
  }

  override def receive: Receive = uninitialized

  override val supervisorStrategy: SupervisorStrategy = OneForOneStrategy() {
    errorDecider.orElse(SupervisorStrategy.defaultDecider)
  }

  /** Method defining the supervising behavior in case of errors.
    *
    * Child [[MessageHandler]] actor maintains a state that will be lost if
    * the actor is restarted (default akka supervising behavior). Instead of
    * restarting we log the error and allow it to continue handling messages.
    */
  private def errorDecider: SupervisorStrategy.Decider = {
    case error: Exception =>
      logger.warn("Resuming after error.", error)
      SupervisorStrategy.Resume
  }

  private def uninitialized: Receive = {
    case Initialize =>
      val clientId    = UUID.randomUUID()
      val clientActor = clientControllerFactory.createClientController(clientId)

      val messageHandler =
        context.actorOf(
          Props(new MessageHandler(protocol, clientActor)),
          s"message-handler-$clientId"
        )
      clientActor ! JsonRpcServer.WebConnect(messageHandler)
      context.become(initialized(messageHandler))
      unstashAll()

    case _ =>
      stash()
  }

  private def initialized(messageHandler: ActorRef): Receive = { case message =>
    messageHandler.forward(message)
  }
}

object MessageHandlerSupervisor {

  case object Initialize
}
