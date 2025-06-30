package org.enso.polyglot.runtime

import com.github.plokhotnyuk.jsoniter_scala.macros.named
import org.enso.logger.masking.{MaskedPath, MaskedString, ToLogString}
import org.enso.polyglot.runtime.Runtime.Api.{ExpressionId, StackTraceElement}
import org.enso.text.editing.model

import java.io.File

sealed trait ExecutionResult extends ToLogString {

  /** Checks if this result represents a critical failure. * */
  def isFailure: Boolean

  /** Checks if this result represents a non-critical error. * */
  def isError: Boolean
}
object ExecutionResult {

  /** A diagnostic object produced as a compilation outcome, like error or
    * warning.
    *
    * @param kind the diagnostic type
    * @param message the diagnostic message
    * @param file the location of a file
    * @param location the location of the diagnostic object in a file
    * @param expressionId the id of related expression
    * @param stack the stack trace
    */
  @named("executionOutcomeDiagnostic")
  case class Diagnostic(
    kind: DiagnosticType,
    message: Option[String],
    file: Option[File],
    location: Option[model.Range],
    expressionId: Option[ExpressionId],
    stack: Vector[StackTraceElement]
  ) extends ExecutionResult {

    /** @inheritdoc */
    override def toLogString(shouldMask: Boolean): String =
      "Diagnostic(" +
      s"kind=$kind," +
      s"message=${message.map(m => MaskedString(m).toLogString(shouldMask))}," +
      s"file=${file.map(f => MaskedPath(f.toPath).toLogString(shouldMask))}," +
      s"location=$location," +
      s"expressionId=$expressionId," +
      s"stack=${stack.map(_.toLogString(shouldMask))}" +
      ")"

    override def isFailure: Boolean = false

    override def isError: Boolean = kind == DiagnosticType.Error
  }

  object Diagnostic {

    /** Create an error diagnostic message.
      *
      * @param message the diagnostic message
      * @param file the location of a file
      * @param location the location of the diagnostic object in a file
      * @param expressionId the id of related expression
      * @param stack the stack trace
      * @return the instance of an error [[Diagnostic]] message
      */
    def error(
      message: String,
      file: Option[File]                 = None,
      location: Option[model.Range]      = None,
      expressionId: Option[ExpressionId] = None,
      stack: Vector[StackTraceElement]   = Vector()
    ): Diagnostic =
      Diagnostic(
        DiagnosticType.Error,
        Option(message),
        file,
        location,
        expressionId,
        stack
      )

    /** Create a warning diagnostic message.
      *
      * @param message the diagnostic message
      * @param file the location of a file
      * @param location the location of the diagnostic object in a file
      * @param expressionId the id of related expression
      * @param stack the stack trace
      * @return the instance of a warning [[Diagnostic]] message
      */
    def warning(
      message: String,
      file: Option[File],
      location: Option[model.Range]      = None,
      expressionId: Option[ExpressionId] = None,
      stack: Vector[StackTraceElement]   = Vector()
    ): Diagnostic =
      Diagnostic(
        DiagnosticType.Warning,
        Option(message),
        file,
        location,
        expressionId,
        stack
      )
  }

  /** A critical failure when attempting to execute a context.
    *
    * @param message the error message
    * @param file the location of a file producing the error
    */
  @named("executionOutcomeFailure")
  case class Failure(message: String, file: Option[File])
      extends ExecutionResult {

    /** @inheritdoc */
    override def toLogString(shouldMask: Boolean): String =
      s"Failure(message=$message,file=" +
      file.map(f => MaskedPath(f.toPath).toLogString(shouldMask)) +
      ")"

    override def isFailure: Boolean = true

    override def isError: Boolean = true
  }

}
