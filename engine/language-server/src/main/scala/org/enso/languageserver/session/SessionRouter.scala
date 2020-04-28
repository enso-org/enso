package org.enso.languageserver.session

import akka.actor.{Actor, Props}
import org.enso.languageserver.data.ClientId
import org.enso.languageserver.event.{
  DataSessionInitialized,
  DataSessionTerminated,
  RpcSessionInitialized,
  RpcSessionTerminated,
  SessionEvent
}
import org.enso.languageserver.session.SessionRouter.{
  DeliverToDataController,
  DeliverToRpcController
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
    case RpcSessionInitialized(s @ RpcSession(clientId, _)) =>
      context.become(
        running(
          sessions + (clientId -> sessions(clientId).attachRpcSession(s))
        )
      )

    case RpcSessionTerminated(RpcSession(clientId, _)) =>
      val updatedSessions =
        (sessions + (clientId -> sessions(clientId).detachRpcSession()))
          .filterNot(_._2.isSessionTerminated)

      context.become(running(updatedSessions))

    case DataSessionInitialized(s @ DataSession(clientId, _)) =>
      context.become(
        running(
          sessions + (clientId -> sessions(clientId).attachDataSession(s))
        )
      )

    case DataSessionTerminated(DataSession(clientId, _)) =>
      val updatedSessions =
        (sessions + (clientId -> sessions(clientId).detachDataSession()))
          .filterNot(_._2.isSessionTerminated)

      context.become(running(updatedSessions))

    case DeliverToRpcController(clientId, payload) =>
      sessions
        .get(clientId)
        .foreach(_.maybeRpcSession.foreach(_.rpcController ! payload))

    case DeliverToDataController(clientId, payload) =>
      sessions
        .get(clientId)
        .foreach(_.maybeDataSession.foreach(_.dataController ! payload))
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
  case class DeliverToRpcController[A](clientId: ClientId, payload: A)

  /**
    * A command used to deliver an arbitrary `payload` to a data controller
    * identified by the client id.
    *
    * @param clientId the internal client identifier
    * @param payload a payload that is delivered to the controller
    * @tparam A a type of payload
    */
  case class DeliverToDataController[A](clientId: ClientId, payload: A)

  /**
    * Creates configuration object used to create a [[SessionRouter]].
    *
    * @return a configuration object
    */
  def props(): Props = Props(new SessionRouter)

}
