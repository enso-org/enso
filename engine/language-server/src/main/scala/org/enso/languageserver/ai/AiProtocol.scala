package org.enso.languageserver.ai

object AiProtocol {

  sealed trait AiCompletionResult

  case object AiCompletionResult {

    sealed case class Success(fn: String, fnCall: String)
        extends AiCompletionResult

    sealed case class Failure(reason: String) extends AiCompletionResult
  }

  case class AiEvalRequest(reason: String, code: String)

  case class CompletionsMessage(role: String, content: String)

  case class AiCompletionProgressNotification(code: String, reason: String)
}
