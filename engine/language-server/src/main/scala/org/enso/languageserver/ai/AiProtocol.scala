package org.enso.languageserver.ai

import java.util.UUID

object AiProtocol {

  /** Base trait for the AI completion results. */
  sealed trait AiCompletionResult

  case object AiCompletionResult {

    /** Successful completion result.
      *
      * @param fn the code for the function returning the answer
      * @param fnCall the code how to call the function
      */
    sealed case class Success(fn: String, fnCall: String)
        extends AiCompletionResult

    /** Failed completion result
      *
      * @param reason the explanation why the AI was unable to provide the result.
      */
    sealed case class Failure(reason: String) extends AiCompletionResult
  }

  /** The request from AI to evaluate an expression.
    *
    * @param reason the explanation why the AI requires this information
    * @param code the expression code
    */
  case class AiEvalRequest(reason: String, code: String)

  /** The message sent to AI.
    *
    * @param role the AI role
    * @param content the message content
    */
  case class CompletionsMessage(role: String, content: String)

  /** The progress notification sent when AI requests to evaluate an expression.
    *
    * @param code the code that AI requested to evaluate
    * @param reason the explanation why AI requires this information
    * @param visualizationId the id of the visualization being executed. When evaluated,
    * the visualization update will contain the result of the executed expression.
    */
  case class AiCompletionProgressNotification(
    code: String,
    reason: String,
    visualizationId: UUID
  )
}
