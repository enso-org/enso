package org.enso.compiler.pass.analyse

import org.enso.common.CachePreferences
import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.ir.CallArgument.Specified
import org.enso.compiler.core.{CompilerError, ExternalID}
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Expression,
  Module,
  Name,
  Type
}
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.expression.{Application, Comment, Error}
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.IRProcessingPass
import org.enso.compiler.pass.desugar._

import java.util.UUID

/** This pass implements the preference analysis for caching.
  *
  * The pass assigns preferences to the expressions. To mark the expression for
  * caching, it should be added to the [[CachePreferences]] configuration.
  */
case object CachePreferenceAnalysis extends IRPass {

  override type Metadata = WeightInfo

  /** Run desugaring passes first. */
  override lazy val precursorPasses: Seq[IRProcessingPass] = List(
    ComplexType,
    FunctionBinding,
    GenerateMethodBodies,
    LambdaShorthandToLambda,
    OperatorToFunction,
    SectionsToBinOp.INSTANCE
  )

  override lazy val invalidatedPasses: Seq[IRProcessingPass] = List()

  /** Performs the cache preference analysis on a module.
    *
    * @param ir the IR to process
    * @param moduleContext a context that contains information needed to process
    * a module
    * @return ir annotated with the preference information
    */
  override def runModule(
    ir: Module,
    moduleContext: ModuleContext
  ): Module = {
    val weights = WeightInfo()
    ir.copy(bindings = ir.bindings.map(analyseModuleDefinition(_, weights)))
      .updateMetadata(new MetadataPair(this, weights))
  }

  /** Performs the cache preference analysis on an inline expression.
    *
    * @param ir the IR to process
    * @param inlineContext a context object that contains the information needed
    * for inline evaluation
    * @return ir annotated with the preference information
    */
  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression =
    analyseExpression(ir, WeightInfo())

  // === Pass Internals =======================================================

  /** Performas preference analysis on a module definition.
    *
    * @param binding the binding to perform dataflow analysis on
    * @param weights the weights information for the module
    * @return `binding`, with attached preference information
    */
  def analyseModuleDefinition(
    binding: Definition,
    weights: WeightInfo
  ): Definition =
    binding match {
      case _: Definition.Type => binding
      case method: definition.Method.Conversion =>
        method
          .copy(body = analyseExpression(method.body, weights))
          .updateMetadata(new MetadataPair(this, weights))
      case method @ definition.Method
            .Explicit(_, body, _, _, _) =>
        method
          .copy(body = analyseExpression(body, weights))
          .updateMetadata(new MetadataPair(this, weights))
      case _: definition.Method.Binding =>
        throw new CompilerError(
          "Sugared method definitions should not occur during cache " +
          "preference analysis."
        )
      case _: Definition.SugaredType =>
        throw new CompilerError(
          "Complex type definitions should not be present during cache " +
          "preference analysis."
        )
      case _: Comment.Documentation =>
        throw new CompilerError(
          "Documentation should not exist as an entity during cache " +
          "preference analysis."
        )
      case _: Type.Ascription =>
        throw new CompilerError(
          "Type signatures should not exist at the top level during " +
          "cache preference analysis."
        )
      case _: Name.BuiltinAnnotation =>
        throw new CompilerError(
          "Annotations should already be associated by the point of " +
          "cache preference analysis."
        )
      case ann: Name.GenericAnnotation => ann
      case err: Error                  => err
    }

  /** Performs preference analysis on an arbitrary expression.
    *
    * @param expression the expression to perform the analysis on
    * @param weights the weights information for the module
    * @return `expression`, with attached preference information
    */
  def analyseExpression(
    expression: Expression,
    weights: WeightInfo
  ): Expression = {
    expression.transformExpressions {
      case binding: Expression.Binding =>
        binding.expression.getExternalId
          .foreach(weights.update(_, CachePreferences.Kind.BINDING_EXPRESSION))
        binding
          .copy(
            name       = binding.name.updateMetadata(new MetadataPair(this, weights)),
            expression = analyseExpression(binding.expression, weights)
          )
          .updateMetadata(new MetadataPair(this, weights))
      case error: Error =>
        error
      case app: Application.Prefix =>
        app.arguments match {
          case self :: rest =>
            val newSelf = analyseSelfCallArgument(self, weights)
            app.copy(arguments = newSelf :: rest)
          case _ =>
            app
        }
      case expr =>
        expr
          .mapExpressions(analyseExpression(_, weights))
          .updateMetadata(new MetadataPair(this, weights))
    }
  }

  private def analyseSelfCallArgument(
    callArgument: CallArgument,
    weights: WeightInfo
  ): CallArgument = {
    callArgument.value.getExternalId
      .foreach(weights.update(_, CachePreferences.Kind.SELF_ARGUMENT))
    callArgument match {
      case arg: Specified =>
        arg.copy(value =
          analyseExpression(arg.value, weights)
            .updateMetadata(new MetadataPair(this, weights))
        )
    }
  }

  /** Performs preference analysis on a function definition argument.
    *
    * @param argument the definition argument to perform the analysis on
    * @param weights the weights information for the module
    * @return `argument`, with attached preference information
    */
  def analyseDefinitionArgument(
    argument: DefinitionArgument,
    weights: WeightInfo
  ): DefinitionArgument = {
    argument match {
      case spec @ DefinitionArgument.Specified(_, _, defValue, _, _, _) =>
        spec
          .copy(defaultValue = defValue.map(analyseExpression(_, weights)))
          .updateMetadata(new MetadataPair(this, weights))
    }
  }

  // === Pass Metadata ========================================================

  /** Storage for a preference information.
    *
    * @param weights the storage for weights of the program components
    */
  sealed case class WeightInfo(
    preferences: CachePreferences = CachePreferences.empty()
  ) extends IRPass.IRMetadata {

    /** The name of the metadata as a string. */
    override val metadataName: String = "CachePreferenceAnalysis.Weights"

    /** @inheritdoc */
    override def prepareForSerialization(compiler: Compiler): WeightInfo = this

    /** @inheritdoc */
    override def restoreFromSerialization(
      compiler: Compiler
    ): Option[WeightInfo] = Some(this)

    /** Assign the weight to an id.
      *
      * @param id the external id
      * @param kind the assigned expression kind
      */
    def update(id: UUID @ExternalID, kind: CachePreferences.Kind): Unit =
      preferences.set(id, kind)

    override def duplicate(): Option[IRPass.IRMetadata] = {
      Some(copy(preferences = this.preferences.copy()))
    }
  }
}
