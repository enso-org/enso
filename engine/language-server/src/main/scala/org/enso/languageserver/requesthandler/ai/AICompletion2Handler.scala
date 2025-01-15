package org.enso.languageserver.requesthandler.ai

import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.OAuth2BearerToken
import akka.pattern.PipeToSupport
import akka.stream.Materializer
import akka.util.ByteString
import com.typesafe.scalalogging.LazyLogging
import io.circe.Json
import io.circe.syntax._
import io.circe.generic.auto._
import org.enso.jsonrpc._
import org.enso.languageserver.ai.AiApi.{
  AiCompletion2,
  AiEvaluationError,
  AiHttpError
}
import org.enso.languageserver.ai.AiProtocol
import org.enso.languageserver.data.AICompletionConfig
import org.enso.languageserver.requesthandler.UnsupportedHandler
import org.enso.languageserver.runtime.{
  ContextRegistryProtocol,
  RuntimeFailureMapper
}
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.util.UnhandledLogging
import org.enso.logging.utils.akka.ActorMessageLogging
import org.enso.polyglot.runtime.Runtime.Api

import java.nio.charset.StandardCharsets
import java.util.UUID

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

class AICompletion2Handler(
  cfg: AICompletionConfig,
  session: JsonSession,
  runtime: ActorRef
) extends Actor
    with LazyLogging
    with ActorMessageLogging
    with UnhandledLogging
    with PipeToSupport {

  import AICompletion2Handler._

  override def preStart(): Unit = {
    super.preStart()

    context.system.eventStream.subscribe(self, classOf[Api.VisualizationUpdate])
    context.system.eventStream
      .subscribe(self, classOf[Api.VisualizationEvaluationFailed])
  }

  override def receive: Receive = requestStage

  private val http                        = Http(context.system)
  implicit val ec: ExecutionContext       = context.dispatcher
  implicit val materializer: Materializer = Materializer(context)

  private def requestStage: Receive = LoggingReceive.withLabel("requestStage") {
    case Request(
          AiCompletion2,
          id,
          AiCompletion2.Params(
            contextId,
            expressionId,
            prompt,
            systemPrompt,
            model
          )
        ) =>
      val messages = Vector(
        AiProtocol.CompletionsMessage(
          "system",
          systemPrompt.getOrElse(SYSTEM_PROMPT)
        ),
        AiProtocol.CompletionsMessage("user", prompt)
      )
      val httpReq   = sendHttpRequest(messages, model)
      val debugInfo = DebugInfo(httpReq)

      context.become(
        awaitingCompletionResponse(
          id,
          sender(),
          contextId,
          expressionId,
          messages,
          model,
          debugInfo
        )
      )
  }

  private def evalRequestStage(
    id: Id,
    replyTo: ActorRef,
    contextId: Api.ContextId,
    expressionId: Api.ExpressionId,
    messages: Vector[AiProtocol.CompletionsMessage],
    model: Option[String]
  ): Receive = LoggingReceive.withLabel("evalRequestStage") {
    case req @ AiProtocol.AiEvalRequest(reason, code) =>
      val requestId       = UUID.randomUUID()
      val visualizationId = UUID.randomUUID()

      val executeExpression = Api.ExecuteExpression(
        contextId,
        visualizationId,
        expressionId,
        code
      )
      runtime ! Api.Request(requestId, executeExpression)

      session.rpcController ! AiProtocol.AiCompletionProgressNotification(
        code,
        reason,
        visualizationId
      )

      context.become(
        evalResponseStage(
          id,
          replyTo,
          contextId,
          expressionId,
          visualizationId,
          req,
          messages,
          model
        )
      )
  }

  private def evalResponseStage(
    id: Id,
    replyTo: ActorRef,
    contextId: Api.ContextId,
    expressionId: Api.ExpressionId,
    visualizationId: Api.VisualizationId,
    request: AiProtocol.AiEvalRequest,
    messages: Vector[AiProtocol.CompletionsMessage],
    model: Option[String]
  ): Receive = LoggingReceive.withLabel("evalResponseStage") {
    case Api.VisualizationUpdate(ctx, data)
        if ctx.visualizationId == visualizationId =>
      val visualizationResult = new String(data, StandardCharsets.UTF_8)
      val message = AiProtocol.CompletionsMessage(
        "user",
        s"EVALUATED:\n${request.code}\n\nOUTPUT:\n$visualizationResult"
      )
      val newMessages = messages :+ message

      val httpReq   = sendHttpRequest(newMessages, model)
      val debugInfo = DebugInfo(httpReq)
      context.become(
        awaitingCompletionResponse(
          id,
          replyTo,
          contextId,
          expressionId,
          newMessages,
          model,
          debugInfo
        )
      )

    case Api.VisualizationEvaluationFailed(ctx, message, _)
        if ctx.visualizationId == visualizationId =>
      val aiError = AiEvaluationError(request.code, message)
      replyTo ! ResponseError(Some(id), aiError)
      stop()

    case error: ContextRegistryProtocol.Failure =>
      replyTo ! ResponseError(Some(id), RuntimeFailureMapper.mapFailure(error))
  }

  private def awaitingCompletionResponse(
    id: Id,
    replyTo: ActorRef,
    contextId: Api.ContextId,
    expressionId: Api.ExpressionId,
    messages: Vector[AiProtocol.CompletionsMessage],
    model: Option[String],
    debugInfo: DebugInfo
  ): Receive = LoggingReceive.withLabel("awaitingCompletionStage") {
    case HttpResponse(StatusCodes.OK, data) =>
      val responseUtf8String = data.utf8String
      logger.trace("AI response:\n{}", responseUtf8String)

      parse(responseUtf8String) match {
        case Some(response) =>
          getResponseKind(response) match {
            case Some("final") =>
              getFinalResult(response).fold {
                val aiError = AiHttpError(
                  "Failed to parse final kind of AI response",
                  debugInfo.httpReq,
                  responseUtf8String
                )
                replyTo ! ResponseError(Some(id), aiError)
              }(success => replyTo ! ResponseResult(AiCompletion2, id, success))
              stop()

            case Some("eval") =>
              getEvalResult(response).fold {
                val aiError = AiHttpError(
                  "Failed to parse eval kind of AI response",
                  debugInfo.httpReq,
                  responseUtf8String
                )
                replyTo ! ResponseError(Some(id), aiError)
                stop()
              } { evalRequest =>
                self ! evalRequest
                context.become(
                  evalRequestStage(
                    id,
                    replyTo,
                    contextId,
                    expressionId,
                    messages,
                    model
                  )
                )
              }

            case Some("fail") =>
              getFailResult(response).fold {
                val aiError = AiHttpError(
                  "Failed to parse fail kind of AI response",
                  debugInfo.httpReq,
                  responseUtf8String
                )
                replyTo ! ResponseError(Some(id), aiError)
              }(fail => replyTo ! ResponseResult(AiCompletion2, id, fail))
              stop()

            case _ =>
              val aiError = AiHttpError(
                "Unknown kind of AI response",
                debugInfo.httpReq,
                responseUtf8String
              )
              replyTo ! ResponseError(Some(id), aiError)
              stop()
          }

        case None =>
          val aiError = AiHttpError(
            "Failed to parse AI response as JSON",
            debugInfo.httpReq,
            data.utf8String
          )
          replyTo ! ResponseError(Some(id), aiError)
          stop()
      }

    case HttpResponse(status, data) =>
      val aiError =
        AiHttpError(
          s"Unknown AI response [${status.value}]",
          debugInfo.httpReq,
          data.utf8String
        )
      replyTo ! ResponseError(Some(id), aiError)
      stop()
  }

  private def sendHttpRequest(
    messages: Vector[AiProtocol.CompletionsMessage],
    modelOption: Option[String]
  ): Json = {
    val body = Json.obj(
      ("model", modelOption.getOrElse(MODEL).asJson),
      ("response_format", Json.obj(("type", "json_object".asJson))),
      ("messages", Json.arr(messages.map(_.asJson): _*))
    )

    logger.trace("AI request:\n{}", body)

    val req =
      HttpRequest(
        uri     = API_OPENAI_URI,
        method  = HttpMethods.POST,
        headers = Seq(headers.Authorization(OAuth2BearerToken(cfg.apiKey))),
        entity  = HttpEntity(ContentTypes.`application/json`, body.noSpaces)
      )

    http
      .singleRequest(req)
      .flatMap(response => {
        response.entity
          .toStrict(FiniteDuration(10, "s"))
          .map(e => {
            HttpResponse(response.status, e.data)
          })
      })
      .pipeTo(self)

    body
  }

  private def stop(): Unit = {
    self ! PoisonPill
  }
}

object AICompletion2Handler {

  private val MODEL          = "gpt-4-turbo-preview"
  private val API_OPENAI_URI = "https://api.openai.com/v1/chat/completions"
  private val SYSTEM_PROMPT =
    """You are a data analyst. You use Python3. Installed libraries: ['pandas'].
      |Your task is to output JSON object with fields:
      |- 'kind': 'final'
      |- 'fn': String, Python function returning what user wants. Always write as generic code as possible that will work even if the input data (e.g. file content) changes. Do not assume any input data exists if not provided with it explicitly. Use your knowledge about the world if the provided data is missing.
      |- 'fnCall': String, Python code that calls the generated function.
      |- 'resultPreview': Code in Python. When evaluated, prints to stdout a preview of the result, e.g. 'Visualization.AI.print("Number 5")'. Make it as generic as possible. It should work even if the input data changes. The string written to stdout should be one-line, as short as possible, and as informative as possible, e.g. 'Table with 50 rows and columns "c1", "c2", and "c3"'. It can assume that the 'fnCall' result is in scope.
      |- 'queryParts': Array of user query divided into either non-editable text, or widgets. The idea is that users can click widgets to change them to other values. Every part should be one of the JSON objects:
      |  * Fields:
      |    a. 'kind': 'text'
      |    b. 'text': Part of the user query that should not be widget. In particular, numbers shoul not be widgets.
      |  * Fields:
      |    a. 'kind': 'dropdown'
      |    b. 'values': list of possible values. For example, if the query contains name of a column in a data set, provide all other column names, like ['columnName1', 'columnName2']. If the query contains a common comparator like 'less than', provide other comparators like ['greater than', 'equal to']. The same applies to other comparators like 'most popular'.
      |
      |If in order to provide the answer you need to investigate what is inside the data, you can run code and be asked again the same question with provided stdout by outputing JSON object with fields:
      |- 'kind': 'eval'
      |- 'code': Python code required to investigate data. The code should write to stdout as little as possible. Use 'Visualization.AI.print' function for printing to stdout. You can only use data you are already provided with, no more data can be provided and you can't ask for more data.
      |- 'reason': Reason why you were not able to provide final code.
      |Always prefer outputting the final code. Use kind "eval" only if you can't output object with kind "final".
      |
      |If you can't provide the answer, because the current data and your knowledge about the world is not enough, output JSON object with fields:
      |- 'kind': 'fail'
      |- 'reason': Reason why you were not able to provide the answer. As short as possible. Do not mention Python nor code, this is information for non-tech users.
      |""".stripMargin

  private case class HttpResponse(status: StatusCode, data: ByteString)

  private case class DebugInfo(
    httpReq: Json
  )

  def props(
    cfg: Option[AICompletionConfig],
    session: JsonSession,
    runtime: ActorRef
  ): Props =
    cfg
      .map(conf => Props(new AICompletion2Handler(conf, session, runtime)))
      .getOrElse(Props(new UnsupportedHandler(AiCompletion2)))

  private def parse(str: String): Option[Json] =
    for {
      response       <- io.circe.parser.parse(str).toOption
      responseObj    <- response.asObject
      choices        <- responseObj("choices")
      choicesArr     <- choices.asArray
      firstChoice    <- choicesArr.headOption
      firstChoiceObj <- firstChoice.asObject
      message        <- firstChoiceObj("message")
      messageObj     <- message.asObject
      content        <- messageObj("content")
      contentString  <- content.asString
      contentJson    <- io.circe.parser.parse(contentString).toOption
    } yield contentJson

  private def getFinalResult(
    response: Json
  ): Option[AiProtocol.AiCompletionResult] =
    for {
      obj          <- response.asObject
      fn           <- obj("fn")
      fnString     <- fn.asString
      fnCall       <- obj("fnCall")
      fnCallString <- fnCall.asString
    } yield AiProtocol.AiCompletionResult.Success(fnString, fnCallString)

  private def getEvalResult(response: Json): Option[AiProtocol.AiEvalRequest] =
    for {
      obj          <- response.asObject
      reason       <- obj("reason")
      reasonString <- reason.asString
      code         <- obj("code")
      codeString   <- code.asString
    } yield AiProtocol.AiEvalRequest(reasonString, codeString)

  private def getFailResult(
    response: Json
  ): Option[AiProtocol.AiCompletionResult] =
    for {
      obj          <- response.asObject
      reason       <- obj("reason")
      reasonString <- reason.asString
    } yield AiProtocol.AiCompletionResult.Failure(reasonString)

  private def getResponseKind(response: Json): Option[String] =
    for {
      obj       <- response.asObject
      key       <- obj("kind")
      keyString <- key.asString
    } yield keyString

}
