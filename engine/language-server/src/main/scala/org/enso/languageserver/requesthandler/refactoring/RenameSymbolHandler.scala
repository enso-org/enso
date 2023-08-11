package org.enso.languageserver.requesthandler.refactoring

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.refactoring.RefactoringApi.RenameSymbol
import org.enso.languageserver.refactoring.RenameFailureMapper
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.ExecutionApi
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID

import scala.concurrent.duration.FiniteDuration

/** A request handler for `refactoring/renameSymbol` commands.
  *
  * @param timeout a request timeout
  * @param runtimeConnector a reference to the runtime connector
  */
class RenameSymbolHandler(
  timeout: FiniteDuration,
  runtimeConnector: ActorRef
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(RenameSymbol, id, params: RenameSymbol.Params) =>
      val payload =
        Api.RenameSymbol(params.module, params.expressionId, params.newName)
      runtimeConnector ! Api.Request(UUID.randomUUID(), payload)
      val cancellable =
        context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(
        responseStage(
          id,
          sender(),
          cancellable
        )
      )
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

    case Api.Response(_, Api.SymbolRenamed(newName)) =>
      replyTo ! ResponseResult(
        RenameSymbol,
        id,
        RenameSymbol.Result(newName)
      )
      cancellable.cancel()
      context.stop(self)

    case Api.Response(_, Api.ModuleNotFound(moduleName)) =>
      replyTo ! ResponseError(
        Some(id),
        ExecutionApi.ModuleNotFoundError(moduleName)
      )
      cancellable.cancel()
      context.stop(self)

    case Api.Response(_, Api.SymbolRenameFailed(error)) =>
      replyTo ! ResponseError(Some(id), RenameFailureMapper.mapFailure(error))
      cancellable.cancel()
      context.stop(self)
  }

}

object RenameSymbolHandler {

  /** Creates configuration object used to create a [[RenameSymbolHandler]].
    *
    * @param timeout request timeout
    * @param runtimeConnector reference to the runtime connector
    */
  def props(
    timeout: FiniteDuration,
    runtimeConnector: ActorRef
  ): Props =
    Props(
      new RenameSymbolHandler(timeout, runtimeConnector)
    )

}
