package org.enso.compiler.phase

import org.enso.compiler.data.BindingsMap
import org.enso.compiler.data.BindingsMap.SymbolRestriction
import org.enso.compiler.pass.analyse.BindingAnalysis
import org.enso.interpreter.runtime.Module

import scala.collection.mutable

class ExportsResolution {
  case class Edge(
    exporter: Node,
    symbols: SymbolRestriction,
    exportee: Node
  )

  case class Node(
    module: Module
  ) {
    var exports: List[Edge]    = List()
    var exportedBy: List[Edge] = List()
  }

  private def getBindings(module: Module): BindingsMap =
    module.getIr.unsafeGetMetadata(
      BindingAnalysis,
      "module without binding analysis in Exports Resolution"
    )

  private def buildGraph(modules: List[Module]): List[Node] = {
    val nodes = mutable.Map[Module, Node](
      modules.map(mod => (mod, Node(mod))): _*
    )
    modules.foreach { module =>
      val exports = getBindings(module).getExportedModules
      val node    = nodes(module)
      node.exports = exports.map {
        case (mod, restriction) => Edge(node, restriction, nodes(mod))
      }
      node.exports.foreach { edge => edge.exportee.exportedBy ::= edge }
    }
    nodes.values.toList
  }

  private def detectCycles(nodes: List[Node]): List[List[Node]] = {
    val visited: mutable.Set[Node]    = mutable.Set()
    val inProgress: mutable.Set[Node] = mutable.Set()
    var foundCycles: List[List[Node]] = List()
    def go(node: Node): Option[(Node, List[Node])] = {
      if (inProgress.contains(node)) {
        Some((node, List()))
      } else if (visited.contains(node)) {
        None
      } else {
        inProgress.add(node)
        val children        = node.exports.map(_.exportee)
        val childrenResults = children.flatMap(go)
        inProgress.remove(node)
        visited.add(node)
        childrenResults match {
          case List() => None
          case (mod, path) :: _ =>
            if (mod == node) {
              foundCycles = (mod :: path) :: foundCycles
              None
            } else {
              Some((mod, node :: path))
            }
        }

      }
    }
    nodes.foreach(go)
    foundCycles
  }

  private def topsort(nodes: List[Node]): List[Node] = {
    val degrees            = mutable.Map[Node, Int]()
    var result: List[Node] = List()
    nodes.foreach { node =>
      degrees(node) = node.exports.length
    }
    while (degrees.nonEmpty) {
      val q     = mutable.Queue[Node]()
      val entry = degrees.find { case (_, deg) => deg == 0 }.get._1
      q.enqueue(entry)
      while (q.nonEmpty) {
        val item = q.dequeue()
        degrees -= item
        item.exportedBy.foreach { edge =>
          degrees(edge.exporter) -= 1
          if (degrees(edge.exporter) == 0) {
            q.enqueue(edge.exporter)
          }
        }
        result ::= item
      }
    }
    result.reverse
  }

  def run(modules: List[Module]): Unit = {
    println("running on modules:")
    modules.foreach(x => println(x.getName))
    val graph  = buildGraph(modules)
    val cycles = detectCycles(graph)
    println("Cycle Analysis")
    cycles.foreach { its =>
      println("CYCLE")
      its.foreach(x => println(x.module.getName))
    }
    if (cycles.nonEmpty) {
      throw new RuntimeException(
        "Error in exports resolution, handle me better"
      )
    }
    val tops = topsort(graph)
    println("TOP:")
    tops.foreach(n => println(n.module.getName))

  }
}
