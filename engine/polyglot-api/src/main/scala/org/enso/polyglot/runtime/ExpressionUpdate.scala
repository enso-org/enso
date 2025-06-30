package org.enso.polyglot.runtime

import com.github.plokhotnyuk.jsoniter_scala.macros.named
import Runtime.Api.FunctionSchema
import Runtime.Api.ExpressionId
import Runtime.Api.ExpressionType
import Runtime.Api.MethodCall
import Runtime.Api.ProfilingInfo
import org.enso.logger.masking.ToLogString

/** An update about the computed expression.
  *
  * @param expressionId the expression id
  * @param expressionType the type of expression
  * @param methodCall the underlying method call of this expression
  * @param profilingInfo profiling information about the execution of this expression
  * @param fromCache whether the value for this expression came from the cache
  * @param typeChanged whether the type of the value or method definition
  * has changed from the one that was cached, if any
  * @param payload an extra information about the computed value
  */
@named("expressionUpdate")
case class ExpressionUpdate(
  expressionId: ExpressionId,
  expressionType: Option[ExpressionType],
  methodCall: Option[MethodCall],
  profilingInfo: Vector[ProfilingInfo],
  fromCache: Boolean,
  typeChanged: Boolean,
  payload: ExpressionUpdate.Payload
)
object ExpressionUpdate {

  /** Base trait for expression payloads. */
  sealed trait Payload
  object Payload {

    /** Indicates that the expression was computed to a value.
      *
      * @param warnings information about attached warnings.
      * @param functionSchema if the value represents a function, the function schema of that function, empty option otherwise
      */
    @named("expressionUpdatePayloadValue")
    case class Value(
      warnings: Option[Value.Warnings]       = None,
      functionSchema: Option[FunctionSchema] = None
    ) extends Payload

    object Value {

      /** Information about warnings associated with the value.
        *
        * @param count the number of attached warnings.
        * @param warning textual representation of the attached warning.
        * @param reachedMaxCount true when reported a maximal number of allowed warnings, false otherwise.
        */
      case class Warnings(
        count: Int,
        warning: Option[String],
        reachedMaxCount: Boolean
      )
    }

    /** Indicates that an expression is pending a computation
      */
    @named("expressionUpdatePayloadPending")
    case class Pending(
      message: Option[String],
      progress: Option[Double],
      wasInterrupted: Boolean = false
    ) extends Payload

    /** Indicates that the expression was computed to an error.
      *
      * @param trace the list of expressions leading to the root error.
      */
    @named("expressionUpdatePayloadDataflowError")
    case class DataflowError(trace: Seq[ExpressionId]) extends Payload

    /** Indicates that the expression failed with the runtime exception.
      *
      * @param message the error message
      * @param trace the stack trace
      */
    @named("expressionUpdatePayloadPanic")
    case class Panic(
      message: String,
      trace: Seq[ExpressionId]
    ) extends Payload
        with ToLogString {

      /** @inheritdoc */
      override def toLogString(shouldMask: Boolean): String =
        s"Panic(message=${if (shouldMask) STUB else message},trace=$trace)"
    }

  }
}
