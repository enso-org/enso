package org.enso.compiler.pass.lint

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.CompilerError
import org.enso.compiler.core.ir.{
  DefinitionArgument,
  Expression,
  Function,
  Module,
  Name
}
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.expression.errors
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
    ir: Module,
    moduleContext: ModuleContext
  ): Module = {
    ir.copy(
      bindings = ir.bindings.map {
        case method: definition.Method.Explicit if isStaticMethod(method) =>
          method.copy(
            body = method.body.transformExpressions(transformSelfToError)
          )
        case method: definition.Method.Binding =>
          throw new CompilerError(
            s"unexpected Method.Binding $method present in pass NoSelfInStatic"
          )
        case binding => binding
      }
    )
  }

  private def transformSelfToError: PartialFunction[Expression, Expression] = {
    case Name.Self(location, false, passData, diagnostics) =>
      errors.Syntax(
        location.get,
        errors.Syntax.InvalidSelfArgUsage,
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
    method: definition.Method
  ): Boolean = {
    def findSelfArgument(
      arguments: List[DefinitionArgument]
    ): Option[DefinitionArgument] = {
      arguments.collectFirst {
        case arg @ DefinitionArgument.Specified(
              Name.Self(_, false, _, _),
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
          case Function.Lambda(
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
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = ir
}
