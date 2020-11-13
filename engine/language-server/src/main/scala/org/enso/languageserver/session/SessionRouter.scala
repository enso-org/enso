package org.enso.languageserver.session

import akka.actor.{Actor, Props}
import org.enso.languageserver.data.ClientId
import org.enso.languageserver.event.{
  BinarySessionInitialized,
  BinarySessionTerminated,
  JsonSessionInitialized,
  JsonSessionTerminated,
  SessionEvent
}
import org.enso.languageserver.session.SessionRouter.{
  DeliverToBinaryController,
  DeliverToJsonController
}

/**
  * A content based router that routes arbitrary messages to the session
  * controller identified by the client id.
  */
class SessionRouter extends Actor {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SessionEvent])
  }

  override def receive: Receive =
    running(Map.empty.withDefault(clientId => Session(clientId, None, None)))

  private def running(sessions: Map[ClientId, Session]): Receive = {
    case JsonSessionInitialized(s @ JsonSession(clientId, _)) =>
      context.become(
        running(
          sessions + (clientId -> sessions(clientId).attachJsonSession(s))
        )
      )

    case JsonSessionTerminated(JsonSession(clientId, _)) =>
      val updatedSessions =
        (sessions + (clientId -> sessions(clientId).detachJsonSession()))
          .filterNot(_._2.isSessionTerminated)

      context.become(running(updatedSessions))

    case BinarySessionInitialized(s @ BinarySession(clientId, _)) =>
      context.become(
        running(
          sessions + (clientId -> sessions(clientId).attachBinarySession(s))
        )
      )

    case BinarySessionTerminated(BinarySession(clientId, _)) =>
      val updatedSessions =
        (sessions + (clientId -> sessions(clientId).detachBinarySession()))
          .filterNot(_._2.isSessionTerminated)

      context.become(running(updatedSessions))

    case DeliverToJsonController(clientId, payload) =>
      sessions
        .get(clientId)
        .foreach(_.maybeJsonSession.foreach(_.rpcController ! payload))

    case DeliverToBinaryController(clientId, payload) =>
      sessions
        .get(clientId)
        .foreach(_.maybeBinarySession.foreach(_.dataController ! payload))
  }

}

object SessionRouter {

  /**
    * A command used to deliver an arbitrary `payload` to a rpc controller
    * identified by the client id.
    *
    * @param clientId the internal client identifier
    * @param payload a payload that is delivered to the controller
    * @tparam A a type of payload
    */
  case class DeliverToJsonController[A](clientId: ClientId, payload: A)

  /**
    * A command used to deliver an arbitrary `payload` to a data controller
    * identified by the client id.
    *
    * @param clientId the internal client identifier
    * @param payload a payload that is delivered to the controller
    * @tparam A a type of payload
    */
  case class DeliverToBinaryController[A](clientId: ClientId, payload: A)

  /**
    * Creates configuration object used to create a [[SessionRouter]].
    *
    * @return a configuration object
    */
  def props(): Props = Props(new SessionRouter)

}
