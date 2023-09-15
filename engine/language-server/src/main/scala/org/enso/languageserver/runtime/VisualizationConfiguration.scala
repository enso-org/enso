package org.enso.languageserver.runtime

import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.enso.logger.masking.ToLogString
import org.enso.polyglot.runtime.Runtime.Api

import java.util.UUID

/** A configuration object for properties of the visualization.
  *
  * @param executionContextId an execution context of the visualization
  * @param expression an expression that creates a visualization
  * @param visualizationModule the name of a module to execute expression at
  */
case class VisualizationConfiguration(
  executionContextId: UUID,
  expression: VisualizationExpression,
  visualizationModule: String
) extends ToLogString {

  /** @inheritdoc */
  override def toLogString(shouldMask: Boolean): String =
    s"VisualizationConfiguration(" +
    s"executionContextId=$executionContextId," +
    s"expression=${expression.toLogString(shouldMask)}," +
    s"visualizationModule=$visualizationModule)"

  /** Convert to corresponding [[Api]] message. */
  def toApi: Api.VisualizationConfiguration =
    Api.VisualizationConfiguration(
      executionContextId  = executionContextId,
      expression          = expression.toApi,
      visualizationModule = visualizationModule
    )

}
object VisualizationConfiguration {

  /** Create a visualization configuration.
    *
    * @param contextId an execution context of the visualization
    * @param module a qualified module name containing the visualization
    * @param expression a visualization expression
    * @return an instance of [[VisualizationConfiguration]]
    */
  def apply(
    contextId: UUID,
    module: String,
    expression: String
  ): VisualizationConfiguration =
    new VisualizationConfiguration(
      contextId,
      VisualizationExpression.Text(module, expression),
      module
    )

  /** Create a visualization configuration.
    *
    * @param contextId an execution context of the visualization
    * @param expression a visualization expression
    * @param positionalArgumentsExpressions the list of arguments that will
    * be passed to the visualization expression
    * @return an instance of [[VisualizationConfiguration]]
    */
  def apply(
    contextId: UUID,
    module: String,
    expression: MethodPointer,
    positionalArgumentsExpressions: Vector[String]
  ): VisualizationConfiguration =
    new VisualizationConfiguration(
      contextId,
      VisualizationExpression.ModuleMethod(
        expression,
        positionalArgumentsExpressions
      ),
      module
    )

  private object CodecField {

    val Arguments = "positionalArgumentsExpressions"

    val Expression = "expression"

    val ExecutionContextId = "executionContextId"

    val VisualizationModule = "visualizationModule"
  }

  /** Json decoder that supports both old and new formats. */
  implicit val decoder: Decoder[VisualizationConfiguration] =
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
            visualizationModule <- cursor
              .downField(CodecField.VisualizationModule)
              .as[String]
            arguments <- cursor
              .downField(CodecField.Arguments)
              .as[Option[Vector[String]]]
          } yield VisualizationConfiguration(
            contextId,
            visualizationModule,
            expression,
            arguments.getOrElse(Vector())
          )

        case Right(expression) =>
          for {
            contextId <- cursor
              .downField(CodecField.ExecutionContextId)
              .as[UUID]
            visualizationModule <- cursor
              .downField(CodecField.VisualizationModule)
              .as[String]
          } yield VisualizationConfiguration(
            contextId,
            visualizationModule,
            expression
          )
      }
    }
}

/** A visualization expression. */
sealed trait VisualizationExpression extends ToLogString {

  /** A qualified module name. */
  def module: String

  /** Convert to corresponding [[Api]] message. */
  def toApi: Api.VisualizationExpression
}
object VisualizationExpression {

  /** Visualization expression represented as a text.
    *
    * @param module a qualified module name containing the expression
    * @param expression an expression that creates a visualization
    */
  case class Text(module: String, expression: String)
      extends VisualizationExpression {

    /** @inheritdoc */
    override def toApi: Api.VisualizationExpression =
      Api.VisualizationExpression.Text(module, expression)

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
    * @param positionalArgumentsExpressions the list of arguments that will
    * be passed to the method
    */
  case class ModuleMethod(
    methodPointer: MethodPointer,
    positionalArgumentsExpressions: Vector[String]
  ) extends VisualizationExpression {

    /** @inheritdoc */
    override val module: String = methodPointer.module

    /** @inheritdoc */
    override def toApi: Api.VisualizationExpression =
      Api.VisualizationExpression.ModuleMethod(
        methodPointer.toApi,
        positionalArgumentsExpressions
      )

    /** @inheritdoc */
    override def toLogString(shouldMask: Boolean): String =
      s"ModuleMethod(methodPointer=$methodPointer,positionalArgumentsExpressions=" +
      (if (shouldMask) STUB else positionalArgumentsExpressions) +
      s")"
  }

  private object CodecField {

    val Type = "type"
  }

  private object PayloadType {

    val Text = "Text"

    val ModuleMethod = "ModuleMethod"
  }

  implicit val encoder: Encoder[VisualizationExpression] =
    Encoder.instance[VisualizationExpression] {
      case text: VisualizationExpression.Text =>
        Encoder[VisualizationExpression.Text]
          .apply(text)
          .deepMerge(Json.obj(CodecField.Type -> PayloadType.Text.asJson))

      case moduleMethod: VisualizationExpression.ModuleMethod =>
        Encoder[VisualizationExpression.ModuleMethod]
          .apply(moduleMethod)
          .deepMerge(
            Json.obj(CodecField.Type -> PayloadType.ModuleMethod.asJson)
          )
    }

  implicit val decoder: Decoder[VisualizationExpression] =
    Decoder.instance { cursor =>
      cursor.downField(CodecField.Type).as[String].flatMap {
        case PayloadType.Text =>
          Decoder[VisualizationExpression.Text].tryDecode(cursor)

        case PayloadType.ModuleMethod =>
          Decoder[VisualizationExpression.ModuleMethod].tryDecode(cursor)
      }
    }

}
