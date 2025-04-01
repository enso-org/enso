package org.enso.compiler
package pass.analyse
package alias.graph

import scala.collection.mutable

/** A graph containing aliasing information for a given root scope in Enso. */
abstract class Graph {
  def deepCopy(
    scope_mapping: mutable.Map[Graph.Scope, Graph.Scope] = mutable.Map()
  ): Graph

  private[graph] def rootScope: Graph.Scope
}

object Graph {

  /** Creates new empty, graph */
  private[graph] def create(): Graph = new GraphImpl()

  /** Creates new graph with provided scope */
  private[graph] def create(scope: Graph.Scope): Graph = new GraphImpl(
    scope.asInstanceOf[GraphImpl.Scope]
  )

  abstract class Scope() {
    def deepCopy(
      mapping: mutable.Map[Graph.Scope, Graph.Scope] = mutable.Map()
    ): Graph.Scope

    def withParent(parentScope: Graph.Scope): Graph.Scope
  }

  /** The type of symbols on the graph. */
  type Symbol = String

  /** The type of identifiers on the graph. */
  type Id = Int

}
