package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.{
  `type`,
  CallArgument,
  Expression,
  IdentifiedLocation,
  Module,
  Name,
  Type
}
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.core.ir.expression.Error
import org.enso.compiler.core.CompilerError
import org.enso.compiler.core.ir.expression.{Application, Operator}
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

  override lazy val precursorPasses: Seq[IRPass] = List(
    IgnoredBindings,
    LambdaShorthandToLambda,
    OperatorToFunction,
    SectionsToBinOp
  )

  override lazy val invalidatedPasses: Seq[IRPass] = List(
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
    ir: Module,
    @unused moduleContext: ModuleContext
  ): Module = {
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
    ir: Expression,
    @unused inlineContext: InlineContext
  ): Expression =
    ir.transformExpressions { case a =>
      resolveExpression(a)
    }

  // === Pass Internals =======================================================

  /** The names of the known typing functions. */
  val knownTypingFunctions: Set[String] = Set(
    Type.Ascription.name,
    Type.Context.name,
    Type.Error.name,
    `type`.Set.Concat.name,
    `type`.Set.Subsumption.name,
    `type`.Set.Equality.name,
    `type`.Set.Union.name,
    `type`.Set.Intersection.name
  )

  /** Performs resolution of typing functions in an arbitrary expression.
    *
    * @param expr the expression to perform resolution in
    * @return `expr`, with any typing functions resolved
    */
  def resolveExpression(expr: Expression): Expression = {
    expr.transformExpressions {
      case asc: Type.Ascription => asc
      case app: Application =>
        val result = resolveApplication(app)
        app
          .getMetadata(DocumentationComments)
          .map(doc =>
            result.updateMetadata(new MetadataPair(DocumentationComments, doc))
          )
          .getOrElse(result)
    }
  }

  /** Performs resolution of typing functions in an application.
    *
    * @param app the application to perform resolution in
    * @return `app`, with any typing functions resolved
    */
  def resolveApplication(app: Application): Expression = {
    app match {
      case pre @ Application.Prefix(fn, arguments, _, _, _, _) =>
        fn match {
          case name: Name if name.name == `type`.Set.Union.name =>
            val members = flattenUnion(app).map(resolveExpression)
            `type`.Set.Union(members, app.location)
          case name: Name if knownTypingFunctions.contains(name.name) =>
            resolveKnownFunction(name, pre.arguments, pre.location, pre)
          case _ =>
            pre.copy(
              function  = resolveExpression(fn),
              arguments = arguments.map(resolveCallArgument)
            )
        }
      case force @ Application.Force(target, _, _, _) =>
        force.copy(target = resolveExpression(target))
      case seq @ Application.Sequence(items, _, _, _) =>
        seq.copy(
          items = items.map(resolveExpression)
        )
      case tSet @ Application.Typeset(expr, _, _, _) =>
        tSet.copy(
          expression = expr.map(resolveExpression)
        )
      case _: Operator =>
        throw new CompilerError(
          "Operators should not be present during typing functions lifting."
        )
    }
  }

  def flattenUnion(expr: Expression): List[Expression] = {
    expr match {
      case Application.Prefix(n: Name, args, _, _, _, _)
          if n.name == `type`.Set.Union.name =>
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
    name: Name,
    arguments: List[CallArgument],
    location: Option[IdentifiedLocation],
    originalIR: IR
  ): Expression = {
    val expectedNumArgs = 2
    val lengthIsValid   = arguments.length == expectedNumArgs
    val argsAreValid    = arguments.forall(isValidCallArg)

    if (lengthIsValid && argsAreValid) {
      val leftArg  = resolveExpression(arguments.head.value)
      val rightArg = resolveExpression(arguments.last.value)

      name.name match {
        case Type.Ascription.name =>
          Type.Ascription(leftArg, rightArg, None, location)
        case Type.Context.name =>
          Type.Context(leftArg, rightArg, location)
        case Type.Error.name =>
          Type.Error(leftArg, rightArg, location)
        case `type`.Set.Concat.name =>
          `type`.Set.Concat(leftArg, rightArg, location)
        case `type`.Set.Subsumption.name =>
          `type`.Set.Subsumption(leftArg, rightArg, location)
        case `type`.Set.Equality.name =>
          `type`.Set.Equality(leftArg, rightArg, location)
        case `type`.Set.Intersection.name =>
          `type`.Set.Intersection(leftArg, rightArg, location)
      }
    } else {
      Error.InvalidIR(originalIR)
    }
  }

  /** Performs resolution of typing functions in a call argument.
    *
    * @param arg the argument to perform resolution in
    * @return `arg`, with any call arguments resolved
    */
  def resolveCallArgument(arg: CallArgument): CallArgument = {
    arg match {
      case spec @ CallArgument.Specified(_, value, _, _, _) =>
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
  def isValidCallArg(arg: CallArgument): Boolean = {
    arg match {
      case CallArgument.Specified(name, _, _, _, _) =>
        name.isEmpty
    }
  }
}
