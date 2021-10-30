package org.enso.languageserver.runtime

import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder}

/** A request to invalidate expressions.
  */
sealed trait InvalidatedExpressions

object InvalidatedExpressions {

  import ExecutionApi._

  /** A request to invalidate all expressions.
    */
  case object All extends InvalidatedExpressions {

    val Value = "all"

    val decoder: Decoder[InvalidatedExpressions] =
      Decoder.instance { cursor =>
        cursor.as[String].flatMap {
          case All.`Value` =>
            Right(All)
          case value =>
            Left(DecodingFailure(s"Unsupported value: $value", cursor.history))
        }
      }
  }

  /** A request to invalidate a list of expressions.
    *
    * @param value the list of expressions to invalidate
    */
  case class Expressions(value: Vector[ExpressionId])
      extends InvalidatedExpressions

  object Expressions {

    val decoder: Decoder[InvalidatedExpressions] =
      Decoder.instance { cursor =>
        cursor.as[Vector[ExpressionId]].map(Expressions(_))
      }
  }

  implicit val encoder: Encoder[InvalidatedExpressions] =
    Encoder.instance[InvalidatedExpressions] {
      case All                => All.Value.asJson
      case Expressions(value) => value.asJson
    }

  implicit val decoder: Decoder[InvalidatedExpressions] =
    All.decoder.or(Expressions.decoder)
}
