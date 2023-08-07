package org.enso.languageserver.requesthandler.refactoring

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import akka.pattern.pipe
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.refactoring.ModuleTextEdits
import org.enso.languageserver.refactoring.RefactoringApi.RenameSymbol
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.RuntimeFailureMapper
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID

import scala.concurrent.duration.FiniteDuration

/** A request handler for `refactoring/renameSymbol` commands.
  *
  * @param runtimeFailureMapper mapper for runtime failures
  * @param timeout a request timeout
  * @param runtimeConnector a reference to the runtime connector
  */
class RenameSymbolHandler(
  runtimeFailureMapper: RuntimeFailureMapper,
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

    case Api.Response(_, Api.SymbolRenamed(edits, newName)) =>
      val moduleTextEdits = edits.map(ModuleTextEdits(_))
      replyTo ! ResponseResult(
        RenameSymbol,
        id,
        RenameSymbol.Result(moduleTextEdits, newName)
      )
      cancellable.cancel()
      context.stop(self)

    case Api.Response(_, error: Api.Error) =>
      runtimeFailureMapper
        .mapApiError(error)
        .map { failure =>
          ResponseError(Some(id), RuntimeFailureMapper.mapFailure(failure))
        }
        .pipeTo(replyTo)
      cancellable.cancel()
      context.stop(self)
  }

}

object RenameSymbolHandler {

  /** Creates configuration object used to create a [[RenameSymbolHandler]].
    *
    * @param runtimeFailureMapper mapper for runtime failures
    * @param timeout request timeout
    * @param runtimeConnector reference to the runtime connector
    */
  def props(
    runtimeFailureMapper: RuntimeFailureMapper,
    timeout: FiniteDuration,
    runtimeConnector: ActorRef
  ): Props =
    Props(
      new RenameSymbolHandler(runtimeFailureMapper, timeout, runtimeConnector)
    )

}
