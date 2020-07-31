package org.enso.compiler.phase

import org.enso.compiler.core.IR
import org.enso.interpreter.runtime.Module
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.analyse.BindingAnalysis

import scala.jdk.CollectionConverters._

/**
  * Builds an IR stub. This is useful for source-less modules (such as
  * [[org.enso.interpreter.runtime.builtin.Builtins]]). Having a stub IR
  * guarantees that other modules can compile against it.
  */
object StubIrBuilder {

  /**
    * Build the stub IR for a given module.
    * @param module the module to build IR for.
    * @return the built stub IR.
    */
  def build(module: Module): IR.Module = {
    val ir = IR.Module(List(), List(), None)
    val definedConstructors: List[BindingsMap.Cons] =
      module.getScope.getConstructors.asScala.toList.map {
        case (name, cons) =>
          BindingsMap.Cons(name, cons.getArity)
      }
    val meta = BindingsMap(
      definedConstructors,
      module
    )
    ir.updateMetadata(BindingAnalysis -->> meta)
  }
}
