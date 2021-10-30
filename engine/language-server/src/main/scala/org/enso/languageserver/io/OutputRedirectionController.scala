package org.enso.languageserver.io

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.data.ClientId
import org.enso.languageserver.event.JsonSessionTerminated
import org.enso.languageserver.io.InputOutputProtocol.{
  OutputAppended,
  RedirectOutput,
  SuppressOutput
}
import org.enso.languageserver.io.ObservableOutputStream.OutputObserver
import org.enso.languageserver.io.OutputRedirectionController.CharOutputAppended
import org.enso.languageserver.session.SessionRouter.DeliverToJsonController
import org.enso.languageserver.util.UnhandledLogging

/** A redirection controller that listen to stdout or stderr data changes and
  * dispatches new output to clients that has redirected stdout or err.
  *
  * @param outputStream stdout or stderr
  * @param outputKind a kind of the output
  * @param sessionRouter used to deliver stdout/stderr updates to subscribed
  *                      clients
  */
class OutputRedirectionController(
  outputStream: ObservableOutputStream,
  outputKind: OutputKind,
  sessionRouter: ActorRef
) extends Actor
    with LazyLogging
    with UnhandledLogging
    with OutputObserver {

  override def preStart(): Unit = {
    outputStream.attach(this)
    context.system.eventStream.subscribe(self, classOf[JsonSessionTerminated])
  }

  override def receive: Receive = running()

  private def running(subscribers: Set[ClientId] = Set.empty): Receive = {
    case CharOutputAppended(output) =>
      subscribers foreach { subscriber =>
        sessionRouter ! DeliverToJsonController(
          subscriber,
          OutputAppended(output, outputKind)
        )
      }

    case RedirectOutput(clientId) =>
      context.become(running(subscribers + clientId))

    case SuppressOutput(clientId) =>
      context.become(running(subscribers - clientId))

    case JsonSessionTerminated(session) =>
      context.become(running(subscribers - session.clientId))
  }

  /** @inheritdoc */
  override def update(output: Array[Byte]): Unit =
    self ! CharOutputAppended(new String(output))

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    super.preRestart(reason, message)
    outputStream.detach(this)
  }

  override def postStop(): Unit = {
    outputStream.detach(this)
  }

}

object OutputRedirectionController {

  private case class CharOutputAppended(output: String)

  /** Creates a configuration object used to create a
    * [[OutputRedirectionController]].
    *
    * @param outputStream stdout or stderr
    * @param outputKind a kind of the output
    * @param sessionRouter used to deliver stdout/stderr updates to subscribed
    *                      clients
    * @return a configuration object
    */
  def props(
    outputStream: ObservableOutputStream,
    outputKind: OutputKind,
    sessionRouter: ActorRef
  ): Props =
    Props(
      new OutputRedirectionController(outputStream, outputKind, sessionRouter)
    )

}
