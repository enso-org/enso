package org.enso.polyglot.runtime

import com.github.plokhotnyuk.jsoniter_scala.macros.named
import org.enso.logger.masking.ToLogString
import org.enso.polyglot.runtime.Runtime.Api.MethodPointer

/** A visualization expression. */
sealed trait VisualizationExpression extends ToLogString {
  def module:                         String
  def positionalArgumentsExpressions: Vector[String]
}
object VisualizationExpression {

  /** Visualization expression represented as a text.
    *
    * @param module a qualified module name containing the expression
    * @param expression an expression that creates a visualization
    * @param positionalArgumentsExpressions the list of arguments that will
    * be passed to the method
    */
  @named("visualizationExpressionText")
  case class Text(
    module: String,
    expression: String,
    positionalArgumentsExpressions: Vector[String]
  ) extends VisualizationExpression {

    /** @inheritdoc */
    override def toLogString(shouldMask: Boolean): String =
      s"Text(module=$module" +
      ",expression=" +
      (if (shouldMask) STUB else expression) +
      ",positionalArgumentsExpressions=" +
      (if (shouldMask) STUB
       else positionalArgumentsExpressions.mkString("[", ",", "]")) +
      ")"
  }

  /** Visualization expression represented as a module method.
    *
    * @param methodPointer a pointer to a method definition
    * @param positionalArgumentsExpressions the list of arguments that will
    * be passed to the method
    */
  @named("visualizationExpressionModuleMethod")
  case class ModuleMethod(
    methodPointer: MethodPointer,
    positionalArgumentsExpressions: Vector[String]
  ) extends VisualizationExpression {

    /** @inheritdoc */
    override val module: String = methodPointer.module

    /** @inheritdoc */
    override def toLogString(shouldMask: Boolean): String =
      s"ModuleMethod(methodPointer=$methodPointer," +
      "positionalArgumentsExpressions=" +
      (if (shouldMask) STUB
       else positionalArgumentsExpressions.mkString("[", ",", "]")) +
      ")"
  }
}
