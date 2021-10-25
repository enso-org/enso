package org.enso.projectmanager.requesthandler

import akka.actor.{Actor, Props}
import org.enso.jsonrpc.Errors.NotImplementedError
import org.enso.jsonrpc.{Request, ResponseError}

/** A handler for unimplemented endpoints that just replies with an error. */
class NotImplementedHandler extends Actor {

  /** @inheritdoc */
  override def receive: Receive = requestStage

  private def requestStage: Receive = { case Request(_, id, _) =>
    sender() ! ResponseError(Some(id), NotImplementedError)
    context.stop(self)
  }
}

object NotImplementedHandler {

  /** Creates a configuration object used to create a [[NotImplementedHandler]].
    */
  def props: Props = Props(new NotImplementedHandler)
}
