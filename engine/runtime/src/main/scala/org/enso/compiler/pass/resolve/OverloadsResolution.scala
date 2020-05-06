package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.IRPass

import scala.annotation.unused

/** This pass performs static detection of method overloads and emits errors
  * at the overload definition site if they are detected. It also checks for
  * overloads of atom contructors using the same rule.
  *
  * Method resolution proceeds in the following order:
  *
  * 1. Methods defined directly on the atom are resolved first.
  * 2. Extension methods in the current scope are resolved next.
  * 3. Extension methods in imported scopes are resolved last.
  *
  * This means that it is possible to shadow an imported extension method with
  * one defined locally, and this does not count as an overload.
  *
  * This pass requires the context to provide:
  *
  * - Nothing
  *
  * It must have the following passes run before it:
  *
  * - [[org.enso.compiler.pass.desugar.GenerateMethodBodies]]
  */
case object OverloadsResolution extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  /** Performs static detection of method overloads within a given module.
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
    var seenAtoms: Set[String]                = Set()
    var seenMethods: Map[String, Set[String]] = Map()

    val atoms = ir.bindings.collect {
      case atom: IR.Module.Scope.Definition.Atom => atom
    }

    val newAtoms: List[IR.Module.Scope.Definition] = atoms.map(atom => {
      if (seenAtoms.contains(atom.name.name)) {
        IR.Error.Redefined.Atom(atom.name, atom.location)
      } else {
        seenAtoms = seenAtoms + atom.name.name
        atom
      }
    })

    val methods = ir.bindings.collect {
      case meth: IR.Module.Scope.Definition.Method =>
        seenMethods = seenMethods + (meth.typeName.name -> Set())
        meth
    }

    val newMethods: List[IR.Module.Scope.Definition] = methods.map(method => {
      if (seenMethods(method.typeName.name).contains(method.methodName.name)) {
        IR.Error.Redefined
          .Method(method.typeName, method.methodName, method.location)
      } else {
        val currentMethods = seenMethods(method.typeName.name)
        seenMethods =
          seenMethods + (method.typeName.name ->
          (currentMethods + method.methodName.name))

        method
      }
    })

    ir.copy(
      bindings = newAtoms ::: newMethods
    )
  }

  /** This pass does nothing for the expression flow.
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
}
