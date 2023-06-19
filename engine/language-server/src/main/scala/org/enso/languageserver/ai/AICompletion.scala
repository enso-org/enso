package org.enso.languageserver.ai

import org.enso.jsonrpc.{HasParams, HasResult, Method}

case object AICompletion extends Method("ai/completion") {
  case class Params(prompt: String, stopSequence: String)
  case class Result(code: String)

  implicit val hasParams: HasParams.Aux[this.type, AICompletion.Params] =
    new HasParams[this.type] {
      type Params = AICompletion.Params
    }

  implicit val hasResult: HasResult.Aux[this.type, AICompletion.Result] =
    new HasResult[this.type] {
      type Result = AICompletion.Result
    }
}
