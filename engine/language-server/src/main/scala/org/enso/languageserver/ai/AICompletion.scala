package org.enso.languageserver.ai

import org.enso.jsonrpc.{HasParams, HasResult, Method}

case object AICompletion extends Method("ai/completion"){
  case class Params(prompt: String, stopSequence: String)
  case class Result(code: String)

  implicit val hasParams = new HasParams[this.type] {
    type Params = AICompletion.Params
  }
  implicit val hasResult = new HasResult[this.type] {
    type Result = AICompletion.Result
  }
}
