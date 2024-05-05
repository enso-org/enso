package org.enso.languageserver.requesthandler.refactoring

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.refactoring.RefactoringApi.RenameSymbol
import org.enso.languageserver.refactoring.{RefactoringApi, RenameFailureMapper}
import org.enso.languageserver.runtime.ExecutionApi
import org.enso.languageserver.util.{ApiHandlerWithRetries, UnhandledLogging}
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

/** A request handler for `refactoring/renameSymbol` commands.
  *
  * @param timeout a request timeout
  * @param runtime a reference to the runtime connector
  */
class RenameSymbolHandler(
  timeout: FiniteDuration,
  runtime: ActorRef
) extends ApiHandlerWithRetries[
      Request[RenameSymbol.type, RenameSymbol.Params],
      Api.SymbolRenamed
    ](runtime, timeout)
    with Actor
    with LazyLogging
    with UnhandledLogging {

  var id: Id = null

  override protected def request(
    msg: Request[RefactoringApi.RenameSymbol.type, RenameSymbol.Params]
  ): Api.Request = {
    val Request(RenameSymbol, _, params: RenameSymbol.Params) = msg
    val payload =
      Api.RenameSymbol(params.module, params.expressionId, params.newName)
    id = msg.id
    Api.Request(UUID.randomUUID(), payload)
  }

  override protected def positiveResponse(
    replyTo: ActorRef,
    msg: Api.SymbolRenamed
  ): Unit = {
    replyTo ! ResponseResult(
      RenameSymbol,
      id,
      RenameSymbol.Result(msg.newName)
    )
  }

  override protected def negativeResponse(
    replyTo: ActorRef,
    error: Api.Error
  )(implicit
    ec: ExecutionContext
  ): Unit = {
    error match {
      case Api.ModuleNotFound(moduleName) =>
        replyTo ! ResponseError(
          Some(id),
          ExecutionApi.ModuleNotFoundError(moduleName)
        )
      case Api.SymbolRenameFailed(error) =>
        replyTo ! ResponseError(
          Some(id),
          RenameFailureMapper.mapFailure(error)
        )
      case _ =>
        logger.error(s"unexpected error response $error")
    }
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
