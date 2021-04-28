package org.enso.languageserver.runtime

import java.nio.ByteBuffer

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.languageserver.runtime.RuntimeConnector.Destroy
import org.enso.polyglot.runtime.Runtime
import org.graalvm.polyglot.io.MessageEndpoint

/** An actor managing a connection to Enso's runtime server.
  */
class RuntimeConnector
    extends Actor
    with ActorLogging
    with UnhandledLogging
    with Stash {

  override def preStart(): Unit = {
    log.info("Starting the runtime connector.")
  }

  override def receive: Receive = {
    case RuntimeConnector.Initialize(engine) =>
      log.info(
        s"Runtime connector established connection with the message endpoint " +
        s"$engine."
      )
      unstashAll()
      context.become(initialized(engine, Map()))
    case _ => stash()
  }

  /** Performs communication between runtime and language server.
    * Requests are sent from language server to runtime,
    * responses are forwarded from runtime to the sender.
    *
    * @param engine endpoint of a runtime
    * @param senders request ids with corresponding senders
    */
  def initialized(
    engine: MessageEndpoint,
    senders: Map[Runtime.Api.RequestId, ActorRef]
  ): Receive = {
    case Destroy => context.stop(self)
    case msg: Runtime.Api.Request =>
      engine.sendBinary(Runtime.Api.serialize(msg))
      msg.requestId.foreach { id =>
        context.become(initialized(engine, senders + (id -> sender())))
      }
    case Runtime.Api.Response(None, msg: Runtime.ApiNotification) =>
      context.system.eventStream.publish(msg)
    case msg: Runtime.Api.Response =>
      msg.correlationId.flatMap(senders.get).foreach(_ ! msg)
      msg.correlationId.foreach { correlationId =>
        context.become(initialized(engine, senders - correlationId))
      }
  }
}

object RuntimeConnector {

  /** Protocol message to pass the runtime connection to the actor.
    *
    * @param engineConnection the open runtime connection.
    */
  case class Initialize(engineConnection: MessageEndpoint)

  /** Protocol message to inform the actor about the connection being closed.
    */
  case object Destroy

  /** Helper for creating instances of the [[RuntimeConnector]] actor.
    *
    * @return a [[Props]] instance for the newly created actor.
    */
  def props: Props =
    Props(new RuntimeConnector)

  /** Endpoint implementation used to handle connections with the runtime.
    *
    * @param actor the actor ref to pass received messages to.
    * @param peerEndpoint the runtime server's connection end.
    */
  class Endpoint(actor: ActorRef, peerEndpoint: MessageEndpoint)
      extends MessageEndpoint {
    override def sendText(text: String): Unit = {}

    override def sendBinary(data: ByteBuffer): Unit =
      Runtime.Api
        .deserializeResponse(data)
        .foreach(actor ! _)

    override def sendPing(data: ByteBuffer): Unit = peerEndpoint.sendPong(data)

    override def sendPong(data: ByteBuffer): Unit = {}

    override def sendClose(): Unit = actor ! RuntimeConnector.Destroy
  }
}
