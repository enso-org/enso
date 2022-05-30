package org.enso.compiler.phase

import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.interpreter.runtime.Module

import scala.collection.immutable.ArraySeq
import scala.jdk.CollectionConverters._

/** Builds an IR stub. This is useful for source-less modules (such as
  * [[org.enso.interpreter.runtime.builtin.Builtins]]). Having a stub IR
  * guarantees that other modules can compile against it.
  */
object StubIrBuilder {

  /** Build the stub IR for a given module.
    * @param module the module to build IR for.
    * @return the built stub IR.
    */
  def build(module: Module): IR.Module = {
    val ir        = IR.Module(List(), List(), List(), None)
    val scope     = module.getScope
    val conses    = scope.getConstructors.asScala
    val consNames = conses.keys.map(_.toLowerCase()).toSet
    val definedConstructors: List[BindingsMap.Cons] =
      conses.toList.map { case (name, cons) =>
        BindingsMap.Cons(
          name,
          cons.getArity,
          allFieldsDefaulted = false,
          builtinType = false,
          variants = ArraySeq.unsafeWrapArray(cons.getVariants).map(_.getName)
        );
      }
    val moduleMethods = Option(scope.getMethods.get(scope.getAssociatedType))
      .map(methods =>
        methods.asScala.keys
          .filter(!consNames.contains(_))
          .map(name => BindingsMap.ModuleMethod(name))
          .toList
      )
      .getOrElse(List())
    val polyglot = scope.getPolyglotSymbols.asScala.keys.toList
      .map(BindingsMap.PolyglotSymbol)
    val exportedBindings = definedConstructors.map(c =>
      (
        c.name.toLowerCase,
        List(BindingsMap.ResolvedConstructor(BindingsMap.ModuleReference.Concrete(module), c))
      )
    ) ++ moduleMethods.map(m =>
      (m.name, List(BindingsMap.ResolvedMethod(BindingsMap.ModuleReference.Concrete(module), m)))
    )
    val meta = BindingsMap(
      definedConstructors,
      polyglot,
      moduleMethods,
      BindingsMap.ModuleReference.Concrete(module)
    )
    meta.exportedSymbols = exportedBindings.toMap
    ir.updateMetadata(BindingAnalysis -->> meta)
  }
}
