package org.enso.compiler
package pass.analyse
package alias.graph

import scala.collection.mutable

/** A graph containing aliasing information for a given root scope in Enso. */
abstract class Graph {
  def deepCopy(
    scope_mapping: mutable.Map[Graph.Scope, Graph.Scope] = mutable.Map()
  ): Graph

  def defLinkFor(id: Graph.Id): Option[Graph.Link]
  def linksFor(id: Graph.Id):   java.util.Set[Graph.Link]

  def getOccurrence(id: Graph.Id): Option[GraphOccurrence]
  def scopeFor(id: Graph.Id):      Option[Graph.Scope]
  def rootScope:                   Graph.Scope

  /** Computes the bindings that are shadowed by the binding with the provided
    * `definition`.
    *
    * Please note that just because [[canShadow]] states that an identifier is
    * _capable_ of shadowing, that does not mean that it is necessarily known
    * to do so.
    *
    * @param definition the definition to find the 'shadowees' of
    * @return the bindings shadowed by `definition`
    */
  def knownShadowedDefinitions(
    definition: GraphOccurrence
  ): Set[GraphOccurrence]
}

object Graph {

  /** Creates new empty, graph */
  private[graph] def create(): Graph = new GraphImpl()

  /** Creates new graph with provided scope */
  private[graph] def create(scope: Graph.Scope): Graph = new GraphImpl(
    scope.asInstanceOf[ScopeImpl]
  )

  abstract class Scope {
    def parent:                                                        Option[Scope]
    def allDefinitions:                                                java.util.Collection[GraphOccurrence.Def]
    def forEachOccurenceDefinition(fn: (GraphOccurrence.Def => Unit)): Unit

    def deepCopy(
      mapping: mutable.Map[Graph.Scope, Graph.Scope] = mutable.Map()
    ): Graph.Scope

    def withParent(parentScope: Graph.Scope): Graph.Scope

    private[compiler] def removeScopeFromParent(): Unit
  }

  /** The type of symbols on the graph. */
  type Symbol = String

  /** The type of identifiers on the graph. */
  type Id = Int

  /** A link in the [[Graph]].
    *
    * The source of the link should always be an [[GraphOccurrence.Use]] while the
    * target of the link should always be an [[GraphOccurrence.Def]].
    *
    * @param source the source ID of the link in the graph
    * @param scopeCount the number of scopes that the link traverses
    * @param target the target ID of the link in the graph
    */
  sealed private[analyse] case class Link(
    source: Id,
    scopeCount: Int,
    target: Id
  ) {}
}
