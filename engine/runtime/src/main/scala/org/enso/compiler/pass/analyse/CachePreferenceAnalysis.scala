package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.{
  DefinitionArgument,
  Expression,
  Module,
  Name,
  Type
}
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.expression.{Comment, Error}
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar._

import java.util
import scala.collection.mutable
import scala.jdk.CollectionConverters._

/** This pass implements the preference analysis for caching.
  *
  * The pass assigns weights to the expressions. The greater the weight, the
  * more preferable the expression for caching.
  *
  * Weights:
  *
  *   - `1` - Right hand side expressions
  */
case object CachePreferenceAnalysis extends IRPass {

  override type Metadata = WeightInfo

  /** Run desugaring passes first. */
  override lazy val precursorPasses: Seq[IRPass] = List(
    ComplexType,
    FunctionBinding,
    GenerateMethodBodies,
    LambdaShorthandToLambda,
    OperatorToFunction,
    SectionsToBinOp
  )

  override lazy val invalidatedPasses: Seq[IRPass] = List()

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
      .updateMetadata(this -->> weights)
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
          .updateMetadata(this -->> weights)
      case method @ definition.Method
            .Explicit(_, body, _, _, _) =>
        method
          .copy(body = analyseExpression(body, weights))
          .updateMetadata(this -->> weights)
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
        binding.getExternalId.foreach(weights.update(_, Weight.Never))
        binding.expression.getExternalId
          .foreach(weights.update(_, Weight.Always))
        binding
          .copy(
            name       = binding.name.updateMetadata(this -->> weights),
            expression = analyseExpression(binding.expression, weights)
          )
          .updateMetadata(this -->> weights)
      case error: Error =>
        error
      case expr =>
        expr.getExternalId.collect {
          case id if !weights.contains(id) => weights.update(id, Weight.Never)
        }
        expr
          .mapExpressions(analyseExpression(_, weights))
          .updateMetadata(this -->> weights)
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
      case spec @ DefinitionArgument.Specified(_, _, defValue, _, _, _, _) =>
        spec
          .copy(defaultValue = defValue.map(analyseExpression(_, weights)))
          .updateMetadata(this -->> weights)
    }
  }

  // === Pass Metadata ========================================================

  /** Storage for a preference information.
    *
    * @param weights the storage for weights of the program components
    */
  sealed case class WeightInfo(
    weights: mutable.HashMap[IR.ExternalId, Double] = mutable.HashMap()
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
      * @param weight the assigned weight
      */
    def update(id: IR.ExternalId, weight: Double): Unit =
      weights.put(id, weight)

    /** Get the weight associated with given id */
    def get(id: IR.ExternalId): Double =
      weights.getOrElse(id, Weight.Never)

    /** Check if the weight is assigned to this id. */
    def contains(id: IR.ExternalId): Boolean =
      weights.contains(id)

    /** @return weights as the Java collection */
    def asJavaWeights: util.Map[IR.ExternalId, java.lang.Double] =
      weights.asJava.asInstanceOf[util.Map[IR.ExternalId, java.lang.Double]]

    override def duplicate(): Option[IRPass.IRMetadata] =
      Some(copy(weights = this.weights))
  }

  /** Weight constants */
  object Weight {

    /** Maximum weight meaning that the program component is always cached. */
    val Always: Double = 1.0

    /** Minimum weight meaning that the program component is never cached. */
    val Never: Double = 0.0
  }
}
