package org.enso.compiler.phase

import org.enso.compiler.core.IR
import org.enso.compiler.pass.analyse.BindingResolution
import org.enso.interpreter.runtime.Module
import org.enso.compiler.core.ir.MetadataStorage._
import scala.jdk.CollectionConverters._

object StubIrBuilder {
  def build(module: Module): IR.Module = {
    val ir = IR.Module(List(), List(), None)
    val definedConstructors: List[BindingResolution.Cons] =
      module.getScope.getConstructors.asScala.toList.map {
        case (name, _) => BindingResolution.Cons(IR.Name.Literal(name, None))
      }
    val definedMethods: List[BindingResolution.Method] =
      module.getScope.getMethods.asScala.toList.flatMap {
        case (cons, methods) =>
          val consName =
            IR.Name.Qualified(List(IR.Name.Literal(cons.getName, None)), None)
          methods.asScala.toList.map {
            case (name, _) =>
              BindingResolution.Method(
                IR.Name
                  .MethodReference(consName, IR.Name.Literal(name, None), None)
              )
          }
      }
    val meta = BindingResolution.LocalBindings(
      definedConstructors,
      definedMethods,
      module
    )
    ir.updateMetadata(BindingResolution -->> meta)
  }
}
