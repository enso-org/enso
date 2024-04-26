package org.enso.languageserver.ai

import io.circe.Json
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
      systemPrompt: Option[String]
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

    case class Params(code: String, reason: String)

    implicit
    val hasParams: HasParams.Aux[this.type, AiCompletionProgress.Params] =
      new HasParams[this.type] {
        type Params = AiCompletionProgress.Params
      }
  }

  case class AiError(override val payload: Option[Json])
      extends Error(-32700, "Ai Response Error")

}
