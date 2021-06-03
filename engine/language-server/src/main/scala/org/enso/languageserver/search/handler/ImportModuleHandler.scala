package org.enso.languageserver.search.handler

import java.util.UUID

import akka.actor.{Actor, ActorRef, Cancellable, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.data.Config
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.runtime.RuntimeFailureMapper
import org.enso.languageserver.search.SearchProtocol
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api

import scala.concurrent.duration.FiniteDuration

/** A request handler for import module command.
  *
  * @param config the language server config
  * @param timeout request timeout
  * @param runtime reference to the runtime connector
  */
final class ImportModuleHandler(
  config: Config,
  timeout: FiniteDuration,
  runtime: ActorRef
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case SearchProtocol.ImportSuggestion(suggestion) =>
      runtime ! Api.Request(
        UUID.randomUUID(),
        Api.ImportSuggestionRequest(suggestion)
      )
      val cancellable =
        context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(responseStage(sender(), cancellable))

    case msg: SearchProtocol.SearchFailure =>
      sender() ! msg
      context.stop(self)
  }

  private def responseStage(
    replyTo: ActorRef,
    cancellable: Cancellable
  ): Receive = {
    case RequestTimeout =>
      replyTo ! RequestTimeout
      context.stop(self)

    case Api.Response(_, Api.ImportSuggestionResponse(module, sym, exports)) =>
      replyTo ! SearchProtocol.ImportResult(
        module,
        sym,
        exports.map(toSearchExport)
      )
      cancellable.cancel()
      context.stop(self)

    case Api.Response(_, error: Api.Error) =>
      replyTo ! RuntimeFailureMapper(config).mapApiError(error)
      cancellable.cancel()
      context.stop(self)
  }

  private def toSearchExport(export: Api.Export): SearchProtocol.Export =
    export match {
      case Api.Export.Unqualified(module) =>
        SearchProtocol.Export.Unqualified(module)
      case Api.Export.Qualified(module, alias) =>
        SearchProtocol.Export.Qualified(module, alias)
    }
}

object ImportModuleHandler {

  /** Creates a configuration object used to create [[ImportModuleHandler]].
    *
    * @param config the language server config
    * @param timeout request timeout
    * @param runtime reference to the runtime conector
    */
  def props(config: Config, timeout: FiniteDuration, runtime: ActorRef): Props =
    Props(new ImportModuleHandler(config, timeout, runtime))
}
