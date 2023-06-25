package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.{Application, IdentifiedLocation}
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse._
import org.enso.compiler.pass.desugar.{
  LambdaShorthandToLambda,
  OperatorToFunction,
  SectionsToBinOp
}
import org.enso.compiler.pass.lint.UnusedBindings

import scala.annotation.unused

/** This pass is responsible for lifting applications of type functions such as
  * `:` and `in` and `!` into their specific IR nodes.
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  */
case object TypeFunctions extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] = List(
    IgnoredBindings,
    LambdaShorthandToLambda,
    OperatorToFunction,
    SectionsToBinOp
  )

  override val invalidatedPasses: Seq[IRPass] = List(
    AliasAnalysis,
    CachePreferenceAnalysis,
    DataflowAnalysis,
    DemandAnalysis,
    TailCall,
    UnusedBindings
  )

  /** Performs typing function resolution on a module.
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: IR.Module,
    @unused moduleContext: ModuleContext
  ): IR.Module = {
    val new_bindings = ir.bindings.map(_.mapExpressions(resolveExpression))
    ir.copy(bindings = new_bindings)
  }

  /** Performs typing function resolution on an expression.
    *
    * @param ir the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: IR.Expression,
    @unused inlineContext: InlineContext
  ): IR.Expression =
    ir.transformExpressions { case a =>
      resolveExpression(a)
    }

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr

  // === Pass Internals =======================================================

  /** The names of the known typing functions. */
  val knownTypingFunctions: Set[String] = Set(
    IR.Type.Ascription.name,
    IR.Type.Context.name,
    IR.Type.Error.name,
    IR.Type.Set.Concat.name,
    IR.Type.Set.Subsumption.name,
    IR.Type.Set.Equality.name,
    IR.Type.Set.Union.name,
    IR.Type.Set.Intersection.name,
    IR.Type.Set.Subtraction.name
  )

  /** Performs resolution of typing functions in an arbitrary expression.
    *
    * @param expr the expression to perform resolution in
    * @return `expr`, with any typing functions resolved
    */
  def resolveExpression(expr: IR.Expression): IR.Expression = {
    expr.transformExpressions {
      case asc: IR.Type.Ascription => asc
      case app: IR.Application =>
        val result = resolveApplication(app)
        app
          .getMetadata(DocumentationComments)
          .map(doc => result.updateMetadata(DocumentationComments -->> doc))
          .getOrElse(result)
    }
  }

  /** Performs resolution of typing functions in an application.
    *
    * @param app the application to perform resolution in
    * @return `app`, with any typing functions resolved
    */
  def resolveApplication(app: IR.Application): IR.Expression = {
    app match {
      case pre @ Application.Prefix(fn, arguments, _, _, _, _) =>
        fn match {
          case name: IR.Name if name.name == IR.Type.Set.Union.name =>
            val members = flattenUnion(app).map(resolveExpression)
            IR.Type.Set.Union(members, app.location)
          case name: IR.Name if knownTypingFunctions.contains(name.name) =>
            resolveKnownFunction(name, pre.arguments, pre.location, pre)
          case _ =>
            pre.copy(
              function  = resolveExpression(fn),
              arguments = arguments.map(resolveCallArgument)
            )
        }
      case force @ Application.Force(target, _, _, _) =>
        force.copy(target = resolveExpression(target))
      case seq @ Application.Literal.Sequence(items, _, _, _) =>
        seq.copy(
          items = items.map(resolveExpression)
        )
      case tSet @ Application.Literal.Typeset(expr, _, _, _) =>
        tSet.copy(
          expression = expr.map(resolveExpression)
        )
      case _: Application.Operator =>
        throw new CompilerError(
          "Operators should not be present during typing functions lifting."
        )
    }
  }

  def flattenUnion(expr: IR.Expression): List[IR.Expression] = {
    expr match {
      case Application.Prefix(n: IR.Name, args, _, _, _, _)
          if n.name == IR.Type.Set.Union.name =>
        args.flatMap(arg => flattenUnion(arg.value))
      case _ => List(expr)
    }
  }

  /** Resolves a known typing function to its IR node.
    *
    * @param prefix the application to resolve
    * @return the IR node representing `prefix`
    */
  def resolveKnownFunction(
    name: IR.Name,
    arguments: List[IR.CallArgument],
    location: Option[IdentifiedLocation],
    originalIR: IR
  ): IR.Expression = {
    val expectedNumArgs = 2
    val lengthIsValid   = arguments.length == expectedNumArgs
    val argsAreValid    = arguments.forall(isValidCallArg)

    if (lengthIsValid && argsAreValid) {
      val leftArg  = resolveExpression(arguments.head.value)
      val rightArg = resolveExpression(arguments.last.value)

      name.name match {
        case IR.Type.Ascription.name =>
          IR.Type.Ascription(leftArg, rightArg, location)
        case IR.Type.Context.name =>
          IR.Type.Context(leftArg, rightArg, location)
        case IR.Type.Error.name =>
          IR.Type.Error(leftArg, rightArg, location)
        case IR.Type.Set.Concat.name =>
          IR.Type.Set.Concat(leftArg, rightArg, location)
        case IR.Type.Set.Subsumption.name =>
          IR.Type.Set.Subsumption(leftArg, rightArg, location)
        case IR.Type.Set.Equality.name =>
          IR.Type.Set.Equality(leftArg, rightArg, location)
        case IR.Type.Set.Intersection.name =>
          IR.Type.Set.Intersection(leftArg, rightArg, location)
        case IR.Type.Set.Subtraction.name =>
          IR.Type.Set.Subtraction(leftArg, rightArg, location)
      }
    } else {
      IR.Error.InvalidIR(originalIR)
    }
  }

  /** Performs resolution of typing functions in a call argument.
    *
    * @param arg the argument to perform resolution in
    * @return `arg`, with any call arguments resolved
    */
  def resolveCallArgument(arg: IR.CallArgument): IR.CallArgument = {
    arg match {
      case spec @ IR.CallArgument.Specified(_, value, _, _, _) =>
        spec.copy(
          value = resolveExpression(value)
        )
    }
  }

  // === Utilities ============================================================

  /** Checks if a call argument is valid for a typing expression.
    *
    * As all typing functions are _operators_ in the source, their arguments
    * must:
    *
    * - Not have a name defined.
    * - Have no suspension info or not be suspended
    *
    * @param arg the argument to check
    * @return `true` if `arg` is valid, otherwise `false`
    */
  def isValidCallArg(arg: IR.CallArgument): Boolean = {
    arg match {
      case IR.CallArgument.Specified(name, _, _, _, _) =>
        name.isEmpty
    }
  }
}
