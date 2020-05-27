package org.enso.compiler.pass.desugar

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.Module.Scope.Definition
import org.enso.compiler.core.IR.Module.Scope.Definition.Method
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass

import scala.annotation.unused

/** Desugars complex type definitions to simple type definitions in the module
  * scope.
  *
  * Note that this pass currently ignores the creation of a function
  * representing the type (e.g. `maybe a = Nothing | Just a` as this does not
  * have a runtime representation at the current time.
  *
  * This pass has no configuration.
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  */
case object ComplexType extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass]   = List()
  override val invalidatedPasses: Seq[IRPass] = List()

  /** Performs desugaring of complex type definitions for a module.
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
  ): IR.Module = ir.copy(
    bindings = ir.bindings.flatMap {
      case typ: Definition.Type => desugarComplexType(typ)
      case b                    => List(b)
    }
  )

  /** An identity operation on an arbitrary expression.
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
  ): IR.Expression = ir

  // === Pass Internals =======================================================

  /** Desugars a complex type definition into a series of top-level definitions.
    *
    * @param typ the type definition to desugar
    * @return the top-level definitions corresponding to the desugaring of `typ`
    */
  def desugarComplexType(
    typ: IR.Module.Scope.Definition.Type
  ): List[IR.Module.Scope.Definition] = {
    val atomDefs = typ.body.collect {
      case d: IR.Module.Scope.Definition.Atom => d
    }
    val atomIncludes = typ.body.collect {
      case n: IR.Name => n
    }
    val namesToDefineMethodsOn = atomIncludes ++ atomDefs.map(_.name)
    val methods = typ.body.collect {
      case b: IR.Expression.Binding => b
      case f: IR.Function.Binding   => f
    }

    if ((atomDefs ::: atomIncludes ::: methods).length != typ.body.length) {
      throw new CompilerError(
        "All bindings in a type definition body should be accounted for."
      )
    }

    val methodDefs = methods.flatMap(genMethodDef(_, namesToDefineMethodsOn))

    atomDefs ::: methodDefs
  }

  /** Generates a method definition from a definition in complex type def body.
    *
    * @param ir the definition to generate a method from
    * @param names the names on which the method is being defined
    * @return `ir` as a method
    */
  def genMethodDef(
    ir: IR,
    names: List[IR.Name]
  ): List[IR.Module.Scope.Definition.Method] = {
    ir match {
      case IR.Expression.Binding(name, expr, location, _, _) =>
        val realExpr = expr match {
          case b @ IR.Expression.Block(_, _, _, suspended, _, _) if suspended =>
            b.copy(suspended = false)
          case _ => expr
        }

        names.map(typeName => {
          Method.Binding(typeName, name, List(), realExpr, location)
        })
      case IR.Function.Binding(name, args, body, location, _, _, _) =>
        names.map(typeName => {
          Method.Binding(typeName, name, args, body, location)
        })
      case _ =>
        throw new CompilerError(
          "Unexpected IR node during complex type desugaring."
        )
    }
  }
}
