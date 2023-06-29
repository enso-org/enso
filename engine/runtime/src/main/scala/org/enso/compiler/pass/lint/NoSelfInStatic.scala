package org.enso.compiler.pass.lint

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.{CompilerError, IR}
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.GenerateMethodBodies

/** This linting pass ensures that `self` argument is not used in static methods.
  *
  * This pass requires the context to provide:
  * - Nothing
  */
object NoSelfInStatic extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] =
    Seq(GenerateMethodBodies)

  override val invalidatedPasses: Seq[IRPass] = List()

  override def runModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    ir.copy(
      bindings = ir.bindings.map {
        case method: IR.Module.Scope.Definition.Method.Explicit
            if isStaticMethod(method) =>
          method.copy(
            body = method.body.transformExpressions(transformSelfToError)
          )
        case method: IR.Module.Scope.Definition.Method.Binding =>
          throw new CompilerError(
            s"unexpected Method.Binding $method present in pass NoSelfInStatic"
          )
        case binding => binding
      }
    )
  }

  private def transformSelfToError
    : PartialFunction[IR.Expression, IR.Expression] = {
    case IR.Name.Self(location, false, passData, diagnostics) =>
      IR.Error.Syntax(
        location.get,
        IR.Error.Syntax.InvalidSelfArgUsage,
        passData,
        diagnostics
      )
  }

  /** A method is static if it is either not defined within a type, or if it does not
    * contain a non-synthetic `self` argument.
    * @param method
    * @return
    */
  private def isStaticMethod(
    method: IR.Module.Scope.Definition.Method
  ): Boolean = {
    def findSelfArgument(
      arguments: List[IR.DefinitionArgument]
    ): Option[IR.DefinitionArgument] = {
      arguments.collectFirst {
        case arg @ IR.DefinitionArgument.Specified(
              IR.Name.Self(_, false, _, _),
              _,
              _,
              _,
              _,
              _,
              _
            ) =>
          arg
      }
    }

    method.typeName match {
      case Some(_) =>
        method.body match {
          case IR.Function.Lambda(
                arguments,
                _,
                _,
                _,
                _,
                _
              ) =>
            findSelfArgument(arguments).isEmpty
          case body =>
            throw new CompilerError(
              s"Method body is not a lambda: $body - should have been transformed to lambda by GenerateMethodBodies pass"
            )
        }
      case None => true
    }
  }

  override def runExpression(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = ir
}
