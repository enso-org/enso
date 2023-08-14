package org.enso.languageserver.requesthandler.runtime

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.editions.LibraryName
import org.enso.jsonrpc._
import org.enso.languageserver.libraries.{
  ComponentGroupsResolver,
  ComponentGroupsValidator,
  LibraryComponentGroup
}
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.RuntimeApi._
import org.enso.languageserver.util.UnhandledLogging
import org.enso.pkg.ComponentGroups
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID

import scala.collection.immutable.ListMap
import scala.concurrent.duration.FiniteDuration

/** A request handler for `runtime/getComponentGroups` commands.
  *
  * @param timeout request timeout
  * @param runtime a reference to the runtime connector
  * @param componentGroupsResolver resolves dependencies between the component
  * groups of different packages
  * @param componentGroupsValidator validates the component groups
  */
class GetComponentGroupsHandler(
  timeout: FiniteDuration,
  runtime: ActorRef,
  componentGroupsResolver: ComponentGroupsResolver,
  componentGroupsValidator: ComponentGroupsValidator
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(
          RuntimeGetComponentGroups,
          id,
          _
        ) =>
      runtime ! Api.Request(UUID.randomUUID(), Api.GetComponentGroupsRequest())
      val cancellable =
        context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(responseStage(id, sender(), cancellable))
  }

  private def responseStage(
    id: Id,
    replyTo: ActorRef,
    cancellable: Cancellable
  ): Receive = {
    case RequestTimeout =>
      logger.error("Request [{}] timed out.", id)
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case Api.Response(_, Api.GetComponentGroupsResponse(componentGroups)) =>
      replyTo ! ResponseResult(
        RuntimeGetComponentGroups,
        id,
        RuntimeGetComponentGroups.Result(
          resolveComponentGroups(componentGroups.to(ListMap))
        )
      )
      cancellable.cancel()
      context.stop(self)
  }

  private def resolveComponentGroups(
    componentGroups: Map[LibraryName, ComponentGroups]
  ): Seq[LibraryComponentGroup] = {
    val validated = componentGroupsValidator.validate(componentGroups)

    validated.collect { case (_, Left(error)) =>
      logValidationError(error)
    }

    val validatedComponents = validated
      .collect { case (libraryName, Right(componentGroups)) =>
        libraryName -> componentGroups
      }
    componentGroupsResolver.resolveComponentGroups(validatedComponents)
  }

  private def logValidationError(
    error: ComponentGroupsValidator.ValidationError
  ): Unit =
    error match {
      case ComponentGroupsValidator.ValidationError
            .InvalidComponentGroups(libraryName, message) =>
        logger.warn(
          s"Validation error. Failed to read library [$libraryName] " +
          s"component groups (reason: $message)."
        )
      case ComponentGroupsValidator.ValidationError
            .DuplicatedComponentGroup(libraryName, moduleReference) =>
        logger.warn(
          s"Validation error. Library [$libraryName] defines duplicate " +
          s"component group [$moduleReference]."
        )
      case ComponentGroupsValidator.ValidationError
            .ComponentGroupExtendsNothing(libraryName, moduleReference) =>
        logger.warn(
          s"Validation error. Library [$libraryName] component group " +
          s"[$moduleReference] extends nothing."
        )
    }
}

object GetComponentGroupsHandler {

  /** Creates configuration object used to create a
    * [[GetComponentGroupsHandler]].
    *
    * @param timeout request timeout
    * @param runtime a reference to the runtime connector
    * @param componentGroupsResolver resolves dependencies between the component
    * groups of different packages
    * @param componentGroupsValidator validates the component groups
    */
  def props(
    timeout: FiniteDuration,
    runtime: ActorRef,
    componentGroupsResolver: ComponentGroupsResolver =
      new ComponentGroupsResolver,
    componentGroupsValidator: ComponentGroupsValidator =
      new ComponentGroupsValidator
  ): Props =
    Props(
      new GetComponentGroupsHandler(
        timeout,
        runtime,
        componentGroupsResolver,
        componentGroupsValidator
      )
    )
}
