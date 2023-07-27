package org.enso.logger.akka

import akka.actor.Actor.Receive
import akka.actor.ActorContext
import org.slf4j.Logger

/** The decorator adding invocation logging to a Receive function of an actor.
  *
  * @param label the label `" in state " + label` appended to each message
  * @param logger the logger that will be used for logging actor messages
  * @param r the receive function of an actor
  * @param context the actor context
  */
class ActorLoggingReceive(
  label: Option[String],
  logger: Logger,
  r: Receive
)(implicit context: ActorContext)
    extends Receive {

  /** @inheritdoc */
  override def isDefinedAt(o: Any): Boolean = {
    val handled = r.isDefinedAt(o)
    val labelText = label match {
      case Some(l) => " in state " + l
      case _       => ""
    }
    val template =
      s"received ${if (handled) "handled" else "unhandled"} {} from {}$labelText"
    logger.trace(template, o, context.sender())
    handled
  }

  /** @inheritdoc */
  override def apply(o: Any): Unit = r(o)
}

object ActorLoggingReceive {

  /** Create the [[ActorLoggingReceive]] decorator adding invocation logging
    * to a Receive function.
    *
    * @param label the label `" in state " + label` appended to each message
    * @param logger the logger
    * @param r the original receive function
    */
  def apply(label: Option[String], logger: Logger, r: Receive)(implicit
    context: ActorContext
  ): Receive = r match {
    case _: ActorLoggingReceive =>
      r
    case _ =>
      new ActorLoggingReceive(label, logger, r)

  }
}
