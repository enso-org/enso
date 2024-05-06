package org.enso.languageserver.ai

import io.circe.Json
import io.circe.syntax._
import org.enso.jsonrpc.{Error, HasParams, HasResult, Method}

import java.util.UUID

case object AiApi {

  case object AiCompletion extends Method("ai/completion") {

    case class Params(prompt: String, stopSequence: String)
    case class Result(code: String)

    implicit val hasParams: HasParams.Aux[this.type, AiCompletion.Params] =
      new HasParams[this.type] {
        type Params = AiCompletion.Params
      }

    implicit val hasResult: HasResult.Aux[this.type, AiCompletion.Result] =
      new HasResult[this.type] {
        type Result = AiCompletion.Result
      }
  }

  case object AiCompletion2 extends Method("ai/completion_v2") {

    case class Params(
      contextId: UUID,
      expressionId: UUID,
      prompt: String,
      systemPrompt: Option[String],
      model: Option[String]
    )
    type Result = AiProtocol.AiCompletionResult

    implicit val hasParams: HasParams.Aux[this.type, AiCompletion2.Params] =
      new HasParams[this.type] {
        type Params = AiCompletion2.Params
      }

    implicit val hasResult: HasResult.Aux[this.type, AiCompletion2.Result] =
      new HasResult[this.type] {
        type Result = AiCompletion2.Result
      }
  }

  case object AiCompletionProgress extends Method("ai/completionProgress") {

    case class Params(code: String, reason: String, visualizationId: UUID)

    implicit
    val hasParams: HasParams.Aux[this.type, AiCompletionProgress.Params] =
      new HasParams[this.type] {
        type Params = AiCompletionProgress.Params
      }
  }

  case class AiHttpError(reason: String, request: Json, response: String)
      extends Error(10001, "Failed to process HTTP response") {

    override val payload: Option[Json] = Some(
      Json.obj(
        ("reason", reason.asJson),
        ("request", request),
        ("response", response.asJson)
      )
    )
  }

  case class AiEvaluationError(expression: String, error: String)
      extends Error(10002, "Failed to execute expression") {

    override val payload: Option[Json] = Some(
      Json.obj(
        ("expression", expression.asJson),
        ("error", error.asJson)
      )
    )
  }
}
