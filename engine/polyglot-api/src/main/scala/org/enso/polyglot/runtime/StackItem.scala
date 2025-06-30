package org.enso.polyglot.runtime

import com.github.plokhotnyuk.jsoniter_scala.macros.named
import org.enso.logger.masking.ToLogString
import org.enso.polyglot.runtime.Runtime.Api.{ExpressionId, MethodPointer}

/** A representation of an executable position in code.
  */
sealed trait StackItem
object StackItem {

  /** A call performed at the top of the stack, to initialize the context.
    */
  @named("explicitCall")
  case class ExplicitCall(
    methodPointer: MethodPointer,
    thisArgumentExpression: Option[String],
    positionalArgumentsExpressions: Vector[String]
  ) extends StackItem
      with ToLogString {

    /** @inheritdoc */
    override def toLogString(shouldMask: Boolean): String =
      s"ExplicitCall(" +
      s"methodPointer=$methodPointer,thisArgumentExpression=" +
      (if (shouldMask) thisArgumentExpression.map(_ => STUB)
       else thisArgumentExpression) +
      ",positionalArgumentExpression=" +
      (if (shouldMask) positionalArgumentsExpressions.map(_ => STUB)
       else positionalArgumentsExpressions) +
      ")"
  }

  /** A call corresponding to "entering a function call". */
  @named("localCall")
  case class LocalCall(expressionId: ExpressionId) extends StackItem
}
