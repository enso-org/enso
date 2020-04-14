package org.enso.languageserver.runtime

import akka.actor.{Actor, ActorLogging, Props}
import org.enso.languageserver.data.{Client, Config}
import org.enso.languageserver.runtime.ExecutionApi.ContextId
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api

/**
  * Event listener is created for the given context. It handles notifications
  * from the runtime and send updates to the client.
  *
  * @param config configuration
  * @param client reference to the client
  * @param contextId exectuion context identifier
  */
final class ContextEventsListener(
  config: Config,
  client: Client,
  contextId: ContextId
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  override def preStart(): Unit = {
    context.system.eventStream
      .subscribe(self, classOf[Api.ExpressionValuesComputed]): Unit
  }

  override def receive: Receive = {
    case Api.ExpressionValuesComputed(`contextId`, apiUpdates) =>
      val updates = apiUpdates.flatMap { update =>
        toRuntimeUpdate(update) match {
          case None =>
            log.error(s"Failed to convert $update")
            None
          case runtimeUpdate =>
            runtimeUpdate
        }
      }
      client.actor ! ContextRegistryProtocol
        .ExpressionValuesComputedNotification(
          contextId,
          updates
        )
  }

  private def toRuntimeUpdate(
    update: Api.ExpressionValueUpdate
  ): Option[ExpressionValueUpdate] = {
    update.methodCall match {
      case None =>
        Some(
          ExpressionValueUpdate(
            update.expressionId,
            update.expressionType,
            update.shortValue,
            None
          )
        )
      case Some(methodCall) =>
        toRuntimePointer(methodCall).map { pointer =>
          ExpressionValueUpdate(
            update.expressionId,
            update.expressionType,
            update.shortValue,
            Some(pointer)
          )
        }
    }
  }

  private def toRuntimePointer(
    pointer: Api.MethodPointer
  ): Option[MethodPointer] =
    config.findRelativePath(pointer.file.toFile).map { relativePath =>
      MethodPointer(
        file          = relativePath,
        definedOnType = pointer.definedOnType,
        name          = pointer.name
      )
    }

}

object ContextEventsListener {

  /**
    * Creates a configuration object used to create a [[ContextEventsListener]].
    *
    * @param config configuration
    * @param client reference to the client
    * @param contextId exectuion context identifier
    */
  def props(config: Config, client: Client, contextId: ContextId): Props =
    Props(new ContextEventsListener(config, client, contextId))
}
