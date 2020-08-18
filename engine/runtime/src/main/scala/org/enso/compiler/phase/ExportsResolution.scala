package org.enso.compiler.phase

import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.interpreter.runtime.Module

import scala.collection.mutable

class ExportsResolution {
  private def getBindings(module: Module): BindingsMap =
    module.getIr.unsafeGetMetadata(
      BindingAnalysis,
      "module without binding analysis in Exports Resolution"
    )

  private def detectCycles(modules: List[Module]): List[List[Module]] = {
    val visited: mutable.Set[Module]    = mutable.Set()
    val inProgress: mutable.Set[Module] = mutable.Set()
    var foundCycles: List[List[Module]] = List()
    def go(module: Module): Option[(Module, List[Module])] = {
      println(s"Running on module: ${module.getName} ${inProgress.contains(module)} ${visited.contains(module)}")
      if (inProgress.contains(module)) {
        Some((module, List()))
      } else if (visited.contains(module)) {
        None
      } else {
        inProgress.add(module)
        val children = getBindings(module).resolvedImports
          .filter(_.exports.isDefined)
          .map(_.module)
        val childrenResults = children.flatMap(go)
        inProgress.remove(module)
        visited.add(module)
        childrenResults match {
          case List() => None
          case (mod, path) :: _ =>
            if (mod == module) {
              foundCycles = (mod :: path) :: foundCycles
              None
            } else {
              Some((mod, module :: path))
            }
        }

      }
    }
    modules.foreach(go)
    foundCycles
  }

  def run(modules: List[Module]): Unit = {
    println("running on modules:")
    modules.foreach(x => println(x.getName))
    val cycles = detectCycles(modules)
    println("Cycle Analysis")
    cycles.foreach { its =>
      println("CYCLE")
      its.foreach(x => println(x.getName))
    }
  }
}
