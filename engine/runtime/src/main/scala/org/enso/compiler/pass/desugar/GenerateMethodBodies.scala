package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition.Method
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.{AliasAnalysis, DataflowAnalysis, TailCall}
import org.enso.compiler.pass.lint.UnusedBindings
import org.enso.compiler.pass.optimise.LambdaConsolidate
import org.enso.interpreter.epb.EpbParser
import org.enso.interpreter.epb.EpbParser.ForeignLanguage
import org.enso.pkg.QualifiedName

import scala.annotation.unused

/** This pass is responsible for ensuring that method bodies are in the correct
  * format.
  *
  * The correct format as far as the rest of the compiler pipeline is concerned
  * is as follows:
  *
  * - The body is a function (lambda)
  * - The body has `self` at the start of its argument list.
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  *
  * It must have the following passes run before it:
  *
  * - [[FunctionBinding]]
  */
case object GenerateMethodBodies extends IRPass {

  /** This is a desugaring pass and performs no analysis */
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] =
    List(ComplexType, FunctionBinding)
  override val invalidatedPasses: Seq[IRPass] = List(
    AliasAnalysis,
    DataflowAnalysis,
    LambdaConsolidate,
    NestedPatternMatch,
    TailCall,
    UnusedBindings
  )

  /** Generates and consolidates method bodies.
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    ir.copy(
      bindings = ir.bindings.map {
        case m: IR.Module.Scope.Definition.Method =>
          processMethodDef(m, moduleContext.module.getName)
        case x => x
      }
    )
  }

  /** Processes a method definition, ensuring that it's in the correct format.
    *
    * @param ir the method definition to process
    * @param moduleName a name of the module that is being processed
    * @return `ir` potentially with alterations to ensure that it's in the
    *         correct format
    */
  def processMethodDef(
    ir: IR.Module.Scope.Definition.Method,
    moduleName: QualifiedName
  ): IR.Module.Scope.Definition.Method = {
    ir match {
      case ir: IR.Module.Scope.Definition.Method.Explicit =>
        ir.copy(
          body = ir.body match {
            case fun: IR.Function => processBodyFunction(fun, moduleName)
            case expression       => processBodyExpression(expression, moduleName)
          }
        )
      case ir: Method.Conversion =>
        ir.copy(
          body = ir.body match {
            case fun: IR.Function => processBodyFunction(fun, moduleName)
            case _ =>
              throw new CompilerError(
                "It should not be possible for a conversion method to have " +
                "an arbitrary expression as a body."
              )
          }
        )
      case _: IR.Module.Scope.Definition.Method.Binding =>
        throw new CompilerError(
          "Method definition sugar should not be present during method body " +
          "generation."
        )
    }
  }

  /** Processes the method body if it's a function.
    *
    * This is solely responsible for prepending the `self` argument to the list
    * of arguments.
    *
    * @param fun the body function
    * @param moduleName a name of the module that is being processed
    * @return the body function with the `self` argument
    */
  def processBodyFunction(
    fun: IR.Function,
    moduleName: QualifiedName
  ): IR.Expression = {
    val selfArgPos = collectChainedFunctionArgs(fun, 0).collect {
      case (arg, idx) if arg.name.isInstanceOf[IR.Name.Self] =>
        (arg, idx)
    }

    selfArgPos match {
      case _ :: (redefined, _) :: _ =>
        IR.Error.Redefined.SelfArg(location = redefined.location)
      case (_, parameterPosition) :: Nil =>
        fun match {
          case lam @ IR.Function.Lambda(_ :: _, _, _, _, _, _)
              if parameterPosition == 0 =>
            lam
          case lam @ IR.Function.Lambda(_, _, _, _, _, _) =>
            fun.addDiagnostic(
              IR.Warning.WrongSelfParameterPos(fun, parameterPosition)
            )
            lam
          case _: IR.Function.Binding =>
            throw new CompilerError(
              "Function definition sugar should not be present during method " +
              "body generation."
            )
        }
      case Nil =>
        fun match {
          case lam @ IR.Function.Lambda(_, body, _, _, _, _)
              if foreignDefinition(
                body,
                lang = Some(ForeignLanguage.JS)
              ).isDefined =>
            insertThisLast(lam, moduleName)
          case lam @ IR.Function.Lambda(_, _, _, _, _, _) =>
            lam
          case _: IR.Function.Binding =>
            throw new CompilerError(
              "Function definition sugar should not be present during method " +
              "body generation."
            )
        }
    }
  }

  private def insertThisLast(
    lam: IR.Function.Lambda,
    moduleName: QualifiedName
  ): IR.Function.Lambda = {
    lam.body match {
      case _: IR.Foreign.Definition =>
        lam.copy(
          arguments = lam.arguments ::: List(genThisArgument(moduleName))
        )
      case body: IR.Function.Lambda =>
        lam.copy(
          body = insertThisLast(body, moduleName)
        )
      case _ =>
        throw new CompilerError("Invalid definition of foreign function")
    }
  }

  /** Processes the method body if it's an expression.
    *
    * @param expr the body expression
    * @return `expr` converted to a function taking the `self` argument
    */
  def processBodyExpression(
    expr: IR.Expression,
    moduleName: QualifiedName
  ): IR.Expression = {
    val args =
      if (foreignDefinition(expr, lang = Some(ForeignLanguage.JS)).nonEmpty)
        genThisArgument(moduleName) :: Nil
      else Nil
    IR.Function.Lambda(
      arguments = args,
      body      = expr,
      location  = expr.location
    )
  }

  /** Generates a definition of the `self` argument for method definitions.
    *
    * @return the `self` argument
    */
  def genThisArgument(moduleName: QualifiedName): IR.DefinitionArgument.Specified = {
    IR.DefinitionArgument.Specified(
      IR.Name.Self(None),
      None,
      defaultValue = Some(IR.Name.Literal(moduleName.item, isMethod = false, None)),
      suspended    = false,
      None
    )
  }

  /** Executes the pass on an expression.
    *
    * It is a identity operation on expressions as method definitions are not
    * expressions.
    *
    * @param ir the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = ir

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr

  /** Collects the argument list of a chain of function definitions.
    *
    * @param function the function to collect args for
    * @return the list of arguments for `function`
    */
  def collectChainedFunctionArgs(
    function: IR.Function,
    idx: Int
  ): List[(IR.DefinitionArgument, Int)] = {
    val argsWithIdx = function.arguments.foldLeft(
      (idx, Nil: List[(IR.DefinitionArgument, Int)])
    ) { case ((i, acc), arg) => (i + 1, (arg, i) :: acc) }

    val bodyArgs = function.body match {
      case f: IR.Function => collectChainedFunctionArgs(f, argsWithIdx._1)
      case _              => List()
    }

    argsWithIdx._2 ::: bodyArgs
  }

  def foreignDefinition(
    body: IR.Expression,
    lang: Option[EpbParser.ForeignLanguage] = None
  ): Option[IR.Foreign.Definition] = {
    body match {
      case foreignDef: IR.Foreign.Definition =>
        lang match {
          case None    => Some(foreignDef)
          case Some(l) => Option.when(l == foreignDef.lang)(foreignDef)
        }
      case fun: IR.Function.Lambda => foreignDefinition(fun.body, lang)
      case _                       => None
    }

  }
}
