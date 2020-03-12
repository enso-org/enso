package org.enso.languageserver.runtime

import java.nio.ByteBuffer
import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import org.enso.languageserver.runtime.RuntimeConnector.Destroy
import org.enso.polyglot.RuntimeApi
import org.graalvm.polyglot.io.MessageEndpoint

/**
  * An actor managing a connection to Enso's runtime server.
  */
class RuntimeConnector extends Actor with ActorLogging with Stash {

  override def receive: Receive = {
    case RuntimeConnector.Initialize(engine) =>
      log.info("Engine connection established.")
      unstashAll()
      context.become(initialized(engine))
    case _ => stash()
  }

  def initialized(engineConnection: MessageEndpoint): Receive = {
    case Destroy => context.stop(self)
    case RuntimeApi.CreateContextResponse(uid) =>
      log.info("Context created {}.", uid)
  }
}

object RuntimeConnector {

  /**
    * Protocol message to pass the runtime connection to the actor.
    *
    * @param engineConnection the open runtime connection.
    */
  case class Initialize(engineConnection: MessageEndpoint)

  /**
    * Protocol message to inform the actor about the connection being closed.
    */
  case object Destroy

  /**
    * Helper for creating instances of the [[RuntimeConnector]] actor.
    *
    * @return a [[Props]] instance for the newly created actor.
    */
  def props: Props =
    Props(new RuntimeConnector)

  /**
    * Endpoint implementation used to handle connections with the runtime.
    *
    * @param actor the actor ref to pass received messages to.
    * @param peerEndpoint the runtime server's connection end.
    */
  class Endpoint(actor: ActorRef, peerEndpoint: MessageEndpoint)
      extends MessageEndpoint {
    override def sendText(text: String): Unit = {}

    override def sendBinary(data: ByteBuffer): Unit =
      RuntimeApi
        .deserialize(data)
        .foreach(actor ! _)

    override def sendPing(data: ByteBuffer): Unit = peerEndpoint.sendPong(data)

    override def sendPong(data: ByteBuffer): Unit = {}

    override def sendClose(): Unit = actor ! RuntimeConnector.Destroy
  }
}
