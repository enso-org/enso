package org.enso.compiler.phase

import org.enso.compiler.core.IR
import org.enso.interpreter.runtime.Module
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.analyse.BindingResolution

import scala.jdk.CollectionConverters._

object StubIrBuilder {
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
    ir.updateMetadata(BindingResolution -->> meta)
  }
}
