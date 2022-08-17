package org.enso.languageserver.runtime

import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.enso.logger.masking.ToLogString
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID

/** A configuration object for properties of the visualisation.
  *
  * @param executionContextId an execution context of the visualisation
  * @param expression an expression that creates a visualisation
  */
case class VisualisationConfiguration(
  executionContextId: UUID,
  expression: VisualisationExpression
) extends ToLogString {

  /** A qualified module name containing the expression. */
  def visualisationModule: String =
    expression.module

  /** @inheritdoc */
  override def toLogString(shouldMask: Boolean): String =
    s"VisualisationConfiguration(" +
    s"executionContextId=$executionContextId," +
    s"expression=${expression.toLogString(shouldMask)})"

  /** Convert to corresponding [[Api]] message. */
  def toApi: Api.VisualisationConfiguration =
    Api.VisualisationConfiguration(
      executionContextId = executionContextId,
      expression         = expression.toApi
    )

}
object VisualisationConfiguration {

  /** Create a visualisation configuration.
    *
    * @param contextId an execution context of the visualisation
    * @param module a qualified module name containing the visualisation
    * @param expression a visualisation expression
    * @return an instance of [[VisualisationConfiguration]]
    */
  def apply(
    contextId: UUID,
    module: String,
    expression: String
  ): VisualisationConfiguration =
    new VisualisationConfiguration(
      contextId,
      VisualisationExpression.Text(module, expression)
    )

  /** Create a visualisation configuration.
    *
    * @param contextId an execution context of the visualisation
    * @param expression a visualisation expression
    * @return an instance of [[VisualisationConfiguration]]
    */
  def apply(
    contextId: UUID,
    expression: MethodPointer
  ): VisualisationConfiguration =
    new VisualisationConfiguration(
      contextId,
      VisualisationExpression.ModuleMethod(expression)
    )

  private object CodecField {

    val Expression = "expression"

    val ExecutionContextId = "executionContextId"

    val VisualisationModule = "visualisationModule"
  }

  /** Json decoder that supports both old and new formats. */
  implicit val decoder: Decoder[VisualisationConfiguration] =
    Decoder.instance { cursor =>
      cursor.downField(CodecField.Expression).as[String] match {
        case Left(_) =>
          for {
            contextId <- cursor
              .downField(CodecField.ExecutionContextId)
              .as[UUID]
            expression <- cursor
              .downField(CodecField.Expression)
              .as[MethodPointer]
          } yield VisualisationConfiguration(contextId, expression)

        case Right(expression) =>
          for {
            contextId <- cursor
              .downField(CodecField.ExecutionContextId)
              .as[UUID]
            visualisationModule <- cursor
              .downField(CodecField.VisualisationModule)
              .as[String]
          } yield VisualisationConfiguration(
            contextId,
            visualisationModule,
            expression
          )
      }
    }
}

/** A visualisation expression. */
sealed trait VisualisationExpression extends ToLogString {

  /** A qualified module name. */
  def module: String

  /** Convert to corresponding [[Api]] message. */
  def toApi: Api.VisualisationExpression
}
object VisualisationExpression {

  /** Visualization expression represented as a text.
    *
    * @param module a qualified module name containing the expression
    * @param expression an expression that creates a visualization
    */
  case class Text(module: String, expression: String)
      extends VisualisationExpression {

    /** @inheritdoc */
    override def toApi: Api.VisualisationExpression =
      Api.VisualisationExpression.Text(module, expression)

    /** @inheritdoc */
    override def toLogString(shouldMask: Boolean): String =
      s"Text(module=$module" +
      s",expression=" +
      (if (shouldMask) STUB else expression) +
      ")"
  }

  /** Visualization expression represented as a module method.
    *
    * @param methodPointer a pointer to a method definition
    */
  case class ModuleMethod(methodPointer: MethodPointer)
      extends VisualisationExpression {

    /** @inheritdoc */
    override val module: String = methodPointer.module

    /** @inheritdoc */
    override def toApi: Api.VisualisationExpression =
      Api.VisualisationExpression.ModuleMethod(methodPointer.toApi)

    /** @inheritdoc */
    override def toLogString(shouldMask: Boolean): String =
      s"ModuleMethod(methodPointer=$methodPointer)"
  }

  private object CodecField {

    val Type = "type"
  }

  private object PayloadType {

    val Text = "Text"

    val ModuleMethod = "ModuleMethod"
  }

  implicit val encoder: Encoder[VisualisationExpression] =
    Encoder.instance[VisualisationExpression] {
      case text: VisualisationExpression.Text =>
        Encoder[VisualisationExpression.Text]
          .apply(text)
          .deepMerge(Json.obj(CodecField.Type -> PayloadType.Text.asJson))

      case moduleMethod: VisualisationExpression.ModuleMethod =>
        Encoder[VisualisationExpression.ModuleMethod]
          .apply(moduleMethod)
          .deepMerge(
            Json.obj(CodecField.Type -> PayloadType.ModuleMethod.asJson)
          )
    }

  implicit val decoder: Decoder[VisualisationExpression] =
    Decoder.instance { cursor =>
      cursor.downField(CodecField.Type).as[String].flatMap {
        case PayloadType.Text =>
          Decoder[VisualisationExpression.Text].tryDecode(cursor)

        case PayloadType.ModuleMethod =>
          Decoder[VisualisationExpression.ModuleMethod].tryDecode(cursor)
      }
    }

}
