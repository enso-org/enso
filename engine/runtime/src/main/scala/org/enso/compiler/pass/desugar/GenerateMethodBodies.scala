package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Error.Redefined
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass

/** This pass is responsible for ensuring that method bodies are in the correct
  * format.
  *
  * The correct format as far as the rest of the compiler pipeline is concerned
  * is as follows:
  *
  * - The body is a function (lambda)
  * - The body has `this` at the start of its argument list.
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

  override val precursorPasses: Seq[IRPass]   = List()
  override val invalidatedPasses: Seq[IRPass] = List()

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
        case m: IR.Module.Scope.Definition.Method => processMethodDef(m)
        case x                                    => x
      }
    )
  }

  /** Processes a method definition, ensuring that it's in the correct format.
    *
    * @param ir the method definition to process
    * @return `ir` potentially with alterations to ensure that it's in the
    *         correct format
    */
  def processMethodDef(
    ir: IR.Module.Scope.Definition.Method
  ): IR.Module.Scope.Definition.Method = {
    ir match {
      case ir: IR.Module.Scope.Definition.Method.Explicit =>
        ir.copy(
          body = ir.body match {
            case fun: IR.Function => processBodyFunction(fun)
            case expression       => processBodyExpression(expression)
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
    * This is solely responsible for prepending the `this` argument to the list
    * of arguments.
    *
    * @param fun the body function
    * @return the body function with the `this` argument
    */
  def processBodyFunction(fun: IR.Function): IR.Expression = {
    val containsThis = collectChainedFunctionArgs(fun).exists(arg =>
      arg.name == IR.Name.This(arg.name.location)
    )

    if (!containsThis) {
      fun match {
        case lam @ IR.Function.Lambda(args, _, _, _, _, _) =>
          lam.copy(
            arguments = genThisArgument :: args
          )
        case _: IR.Function.Binding =>
          throw new CompilerError(
            "Function definition sugar should not be present during method " +
            "body generation."
          )
      }
    } else {
      IR.Error.Redefined.ThisArg(fun.location)
    }
  }

  /** Processes the method body if it's an expression.
    *
    * @param expr the body expression
    * @return `expr` converted to a function taking the `this` argument
    */
  def processBodyExpression(expr: IR.Expression): IR.Expression = {
    IR.Function.Lambda(
      arguments = List(genThisArgument),
      body      = expr,
      location  = expr.location
    )
  }

  /** Generates a definition of the `this` argument for method definitions.
    *
    * @return the `this` argument
    */
  def genThisArgument: IR.DefinitionArgument.Specified = {
    IR.DefinitionArgument.Specified(
      IR.Name.This(None),
      None,
      suspended = false,
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

  /** Collects the argument list of a chain of function definitions.
    *
    * @param function the function to collect args for
    * @return the list of arguments for `function`
    */
  def collectChainedFunctionArgs(
    function: IR.Function
  ): List[IR.DefinitionArgument] = {
    val bodyArgs = function.body match {
      case f: IR.Function => (collectChainedFunctionArgs(f))
      case _              => List()
    }

    function.arguments ::: bodyArgs
  }
}
