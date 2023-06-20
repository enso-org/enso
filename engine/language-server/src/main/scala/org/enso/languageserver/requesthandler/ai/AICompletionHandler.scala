package org.enso.languageserver.requesthandler.ai

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc.{Errors, Id, Request, ResponseError, ResponseResult}
import org.enso.languageserver.ai.AICompletion
import org.enso.languageserver.util.UnhandledLogging
import akka.http.scaladsl.model._
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.headers.OAuth2BearerToken
import akka.pattern.PipeToSupport
import akka.stream.Materializer
import akka.util.ByteString
import io.circe.Json
import org.enso.languageserver.data.AICompletionConfig

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

class AICompletionHandler(cfg: AICompletionConfig)
    extends Actor
    with LazyLogging
    with UnhandledLogging
    with PipeToSupport {
  override def receive: Receive = requestStage

  case class AIResponse(status: StatusCode, data: ByteString)

  val http                                = Http(context.system)
  implicit val ec: ExecutionContext       = context.dispatcher
  implicit val materializer: Materializer = Materializer(context)

  private def requestStage: Receive = {
    case Request(AICompletion, id, AICompletion.Params(prompt, stop)) =>
      val body = Json.fromFields(
        Seq(
          ("model", Json.fromString("text-davinci-003")),
          ("prompt", Json.fromString(prompt)),
          ("stop", Json.fromString(stop)),
          ("temperature", Json.fromDoubleOrNull(0)),
          ("max_tokens", Json.fromInt(64))
        )
      )
      val req =
        HttpRequest(
          uri    = "https://api.openai.com/v1/completions",
          method = HttpMethods.POST,
          headers = Seq(
            headers.Authorization(OAuth2BearerToken(cfg.apiKey))
          ),
          entity = HttpEntity(ContentTypes.`application/json`, body.noSpaces)
        )

      http
        .singleRequest(req)
        .flatMap(response => {
          response.entity
            .toStrict(FiniteDuration(10, "s"))
            .map(e => {
              AIResponse(response.status, e.data)
            })
        })
        .pipeTo(self)
      context.become(awaitingCompletionResponse(id, sender()))
  }

  private def awaitingCompletionResponse(id: Id, replyTo: ActorRef): Receive = {
    case AIResponse(StatusCodes.OK, data) =>
      val response =
        for {
          parsed             <- io.circe.parser.parse(data.utf8String).toOption
          obj                <- parsed.asObject
          choices            <- obj("choices")
          choicesVec         <- choices.asArray
          firstChoice        <- choicesVec.headOption
          firstChoiceObj     <- firstChoice.asObject
          firstChoiceText    <- firstChoiceObj("text")
          firstChoiceTextStr <- firstChoiceText.asString
        } yield ResponseResult(
          AICompletion,
          id,
          AICompletion.Result(firstChoiceTextStr)
        )
      val handledErrors =
        response.getOrElse(ResponseError(Some(id), Errors.ServiceError))
      replyTo ! handledErrors
    case AIResponse(status, data) =>
      replyTo ! ResponseError(
        Some(id),
        Errors.UnknownError(status.intValue(), data.utf8String, None)
      )
  }
}

class UnsupportedHandler extends Actor with LazyLogging with UnhandledLogging {
  override def receive: Receive = { case Request(AICompletion, id, _) =>
    sender() ! ResponseError(
      Some(id),
      Errors.MethodNotFound
    )

  }
}

object AICompletionHandler {
  def props(cfg: Option[AICompletionConfig]): Props = cfg
    .map(conf =>
      Props(
        new AICompletionHandler(conf)
      )
    )
    .getOrElse(Props(new UnsupportedHandler()))
}
