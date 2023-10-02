package org.enso.compiler.pass.resolve

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.{Diagnostic, Expression, Module, Name, Type}
import org.enso.compiler.core.ir.expression.{errors, Comment}
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.{ComplexType, GenerateMethodBodies}

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

  override lazy val precursorPasses: Seq[IRPass] = List(
    ComplexType,
    GenerateMethodBodies
  )
  override lazy val invalidatedPasses: Seq[IRPass] = List()

  /** Performs static detection of method overloads within a given module.
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: Module,
    moduleContext: ModuleContext
  ): Module = {
    var seenTypes: Set[String]                                   = Set()
    var seenMethods: Map[Option[String], Set[(String, Boolean)]] = Map()

    val types = ir.bindings.collect { case tp: Definition.Type =>
      tp
    }
    ir.bindings.collect { case meth: definition.Method.Explicit =>
      seenMethods += meth.typeName.map(_.name) -> Set()
      meth
    }
    val conversionsForType: mutable.Map[Option[String], Set[String]] =
      mutable.Map()

    val newBindings = ir.bindings.map {
      case tp: Definition.Type =>
        if (seenTypes.contains(tp.name.name)) {
          errors.Redefined.Type(tp.name, tp.location)
        } else {
          seenTypes += tp.name.name
          tp
        }

      case method: definition.Method.Explicit =>
        if (
          seenMethods(method.typeName.map(_.name))
            .contains((method.methodName.name, method.isStatic))
        ) {
          errors.Redefined
            .Method(method.typeName, method.methodName, method.location)
        } else {
          types.find(_.name.name.equals(method.methodName.name)) match {
            case Some(clashedAtom) if method.typeName.isEmpty =>
              errors.Redefined.MethodClashWithAtom(
                clashedAtom.name,
                method.methodName,
                method.location
              )
            case _ =>
              val currentMethods: Set[(String, Boolean)] =
                seenMethods(method.typeName.map(_.name))
              seenMethods += (method.typeName.map(_.name) ->
              (currentMethods + ((method.methodName.name, method.isStatic))))
              method
          }
        }

      case m: definition.Method.Conversion =>
        val fromName = m.sourceTypeName.asInstanceOf[Name]
        conversionsForType.get(m.typeName.map(_.name)) match {
          case Some(elems) =>
            if (elems.contains(fromName.name)) {
              errors.Redefined.Conversion(m.typeName, fromName, m.location)
            } else {
              conversionsForType.update(
                m.typeName.map(_.name),
                conversionsForType(m.typeName.map(_.name)) + fromName.name
              )
              m
            }
          case None =>
            conversionsForType.put(m.typeName.map(_.name), Set(fromName.name))
            m
        }

      case diagnostic: Diagnostic      => diagnostic
      case ann: Name.GenericAnnotation => ann
      case _: Type.Ascription =>
        throw new CompilerError(
          "Type ascriptions should not be present during the overloads resolution."
        )
      case _: definition.Method.Binding =>
        throw new CompilerError(
          "Method bindings should not be present during the overloads resolution."
        )
      case _: Name.BuiltinAnnotation =>
        throw new CompilerError(
          "Builtin annotations should not be present during the overloads resolution."
        )
      case _: Comment.Documentation =>
        throw new CompilerError(
          "Documentation comments should not be present during the overloads resolution."
        )
      case _: Definition.SugaredType =>
        throw new CompilerError(
          "Sugared types should not be present during the overloads resolution."
        )
    }

    ir.copy(bindings = newBindings)
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
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = ir

  /** @inheritdoc */

}
