package org.enso.languageserver.requesthandler.io

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.jsonrpc.{Request, ResponseResult, Unused}
import org.enso.languageserver.io.InputOutputApi.FeedStandardInput
import org.enso.languageserver.io.InputOutputProtocol
import org.enso.languageserver.util.UnhandledLogging

/**
  * A request handler for `io/feedStandardInput` commands.
  *
  * @param stdInController the stdin redirection controller
  */
class FeedStandardInputHandler(stdInController: ActorRef)
    extends Actor
    with ActorLogging
    with UnhandledLogging {

  override def receive: Receive = {
    case Request(FeedStandardInput, id, params: FeedStandardInput.Params) =>
      stdInController ! InputOutputProtocol.FeedStandardInput(
        params.input,
        params.isLineTerminated
      )
      sender() ! ResponseResult(FeedStandardInput, id, Unused)
      context.stop(self)
  }

}

object FeedStandardInputHandler {

  /**
    * Creates a configuration object used to create a
    * [[FeedStandardInputHandler]].
    *
    * @param stdInController the stdin redirection controller
    * @return a configuration object
    */
  def props(stdInController: ActorRef): Props =
    Props(new FeedStandardInputHandler(stdInController))

}
