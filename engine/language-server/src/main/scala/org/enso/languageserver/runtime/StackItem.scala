package org.enso.languageserver.runtime

import java.util.UUID

import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.enso.logger.masking.{MaskedString, ToLogString}

/** A representation of an executable position in code.
  */
sealed trait StackItem extends ToLogString

object StackItem {

  /** A call performed at the top of the stack, to initialize the context.
    *
    * @param methodPointer points to a method definition
    * @param thisArgumentExpression optional argument
    * @param positionalArgumentsExpressions positional arguments
    */
  case class ExplicitCall(
    methodPointer: MethodPointer,
    thisArgumentExpression: Option[String],
    positionalArgumentsExpressions: Vector[String]
  ) extends StackItem {

    /** @inheritdoc */
    override def toLogString(shouldMask: Boolean): String =
      "ExplicitCall(" +
      s"methodPointer=$methodPointer," +
      s"thisArgumentExpression=${thisArgumentExpression.map(MaskedString)}," +
      s"positionalArgumentsExpressions=${positionalArgumentsExpressions.map(MaskedString)}" +
      ")"
  }

  /** A call corresponding to "entering a function call".
    *
    * @param expressionId an expression identifier
    */
  case class LocalCall(expressionId: UUID) extends StackItem {

    /** @inheritdoc */
    override def toLogString(shouldMask: Boolean): String =
      s"LocalCall(expressionId=$expressionId)"
  }

  private object CodecField {

    val Type = "type"

    val MethodPointer = "methodPointer"

    val ThisArgumentExpression = "thisArgumentExpression"

    val PositionalArgumentsExpressions = "positionalArgumentsExpressions"

    val ExpressionId = "expressionId"
  }

  private object CodecType {

    val ExplicitCall = "ExplicitCall"

    val LocalCall = "LocalCall"
  }

  implicit val encoder: Encoder[StackItem] =
    Encoder.instance[StackItem] {
      case ExplicitCall(
            methodPointer,
            thisArgumentExpression,
            positionalArgumentsExpressions
          ) =>
        Json.obj(
          CodecField.Type                           -> CodecType.ExplicitCall.asJson,
          CodecField.MethodPointer                  -> methodPointer.asJson,
          CodecField.ThisArgumentExpression         -> thisArgumentExpression.asJson,
          CodecField.PositionalArgumentsExpressions -> positionalArgumentsExpressions.asJson
        )

      case LocalCall(expressionId) =>
        Json.obj(
          CodecField.Type         -> CodecType.LocalCall.asJson,
          CodecField.ExpressionId -> expressionId.asJson
        )
    }

  implicit val decoder: Decoder[StackItem] =
    Decoder.instance { cursor =>
      cursor.downField(CodecField.Type).as[String].flatMap {
        case CodecType.ExplicitCall =>
          for {
            methodPointer <- cursor
              .downField(CodecField.MethodPointer)
              .as[MethodPointer]
            thisArgumentExpression <- cursor
              .downField(CodecField.ThisArgumentExpression)
              .as[Option[String]]
            positionalArgumentsExpressions <- cursor
              .downField(CodecField.PositionalArgumentsExpressions)
              .as[Vector[String]]
          } yield ExplicitCall(
            methodPointer,
            thisArgumentExpression,
            positionalArgumentsExpressions
          )

        case CodecType.LocalCall =>
          for {
            expressionId <- cursor.downField(CodecField.ExpressionId).as[UUID]
          } yield LocalCall(expressionId)
      }
    }
}
