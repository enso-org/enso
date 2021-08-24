package org.enso.lockmanager

import akka.actor.{Actor, Props, Stash}
import org.enso.lockmanager.ActorToHandlerConnector.SetRequestHandler
import org.enso.lockmanager.client.RuntimeServerRequestHandler
import org.enso.polyglot.runtime.Runtime.Api

class ActorToHandlerConnector extends Actor with Stash {

  override def receive: Receive = initializingStage

  private def initializedStage(
    requestHandler: RuntimeServerRequestHandler
  ): Receive = { case response: Api.Response =>
    requestHandler.onResponseReceived(response)
  }

  private def initializingStage: Receive = {
    case SetRequestHandler(handler) =>
      context.become(initializedStage(handler))
    case _ =>
      stash()
  }

}

object ActorToHandlerConnector {
  def props(): Props = Props(
    new ActorToHandlerConnector
  )

  case class SetRequestHandler(
    runtimeServerRequestHandler: RuntimeServerRequestHandler
  )
}
