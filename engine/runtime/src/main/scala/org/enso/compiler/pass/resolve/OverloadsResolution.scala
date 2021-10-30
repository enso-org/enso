package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.{ComplexType, GenerateMethodBodies}

import scala.annotation.unused
import scala.collection.mutable

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
  */
case object OverloadsResolution extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] = List(
    ComplexType,
    GenerateMethodBodies
  )
  override val invalidatedPasses: Seq[IRPass] = List()

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
      case meth: IR.Module.Scope.Definition.Method.Explicit =>
        seenMethods = seenMethods + (meth.typeName.name -> Set())
        meth
    }

    val newMethods: List[IR.Module.Scope.Definition] = methods.map(method => {
      if (seenMethods(method.typeName.name).contains(method.methodName.name)) {
        IR.Error.Redefined
          .Method(method.typeName, method.methodName, method.location)
      } else {
        val currentMethods = seenMethods(method.typeName.name)
        seenMethods = seenMethods + (method.typeName.name ->
        (currentMethods + method.methodName.name))

        method
      }
    })

    val conversionsForType: mutable.Map[String, Set[String]] = mutable.Map()

    val conversions: List[IR.Module.Scope.Definition] = ir.bindings.collect {
      case m: IR.Module.Scope.Definition.Method.Conversion =>
        val fromName = m.sourceTypeName.asInstanceOf[IR.Name]
        conversionsForType.get(m.typeName.name) match {
          case Some(elems) =>
            if (elems.contains(fromName.name)) {
              IR.Error.Redefined.Conversion(m.typeName, fromName, m.location)
            } else {
              conversionsForType.update(
                m.typeName.name,
                conversionsForType(m.typeName.name) + fromName.name
              )
              m
            }
          case None =>
            conversionsForType.put(m.typeName.name, Set(fromName.name))
            m
        }
    }

    ir.copy(
      bindings = newAtoms ::: newMethods ::: conversions
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

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    @unused sourceIr: T,
    copyOfIr: T
  ): T = copyOfIr
}
