package org.enso.logger.akka

import akka.actor.{Actor, ActorContext}
import org.slf4j.LoggerFactory

/** A trait providing functions for logging received actor messages. */
trait ActorMessageLogging { outer: Actor =>

  object LoggingReceive {

    private val logger = LoggerFactory.getLogger(outer.getClass)

    /** Wrap a Receive partial function in a logging enclosure, which logs a
      * message each time before a message is matched. This includes messages
      * which are not handled.
      *
      * {{{
      * def receive = LoggingReceive {
      *   case x => ...
      * }
      * }}}
      */
    def apply(r: Receive)(implicit context: ActorContext): Receive =
      ActorLoggingReceive(None, logger, r)

    /** Create a decorated logger which appends `" in state " + label`
      * to each message it logs.
      */
    def withLabel(label: String)(r: Receive)(implicit
      context: ActorContext
    ): Receive =
      ActorLoggingReceive(Some(label), logger, r)
  }
}
