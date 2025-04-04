package org.enso.compiler
package pass.analyse
package alias.graph

import org.enso.compiler.debug.Debug

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.annotation.unused

/** A graph containing aliasing information for a given root scope in Enso. */
sealed private[graph] class GraphImpl(
  private val rootScopeImpl: Graph.Scope = new ScopeImpl(),
  private var _nextIdCounter: Int        = 0,
  private var links: Set[Graph.Link]     = Set()
) extends Graph {
  private var sourceLinks: Map[GraphImpl.Id, Set[Graph.Link]] =
    new HashMap()
  private var targetLinks: Map[GraphImpl.Id, Set[Graph.Link]] =
    new HashMap()
  private val toScope: java.util.Map[GraphImpl.Id, ScopeImpl] =
    new java.util.HashMap()

  final def rootScope: ScopeImpl =
    this.rootScopeImpl.asInstanceOf[ScopeImpl]

  {
    links.foreach(addSourceTargetLink)
  }

  /** @return the next counter value
    */
  private[graph] def nextIdCounter: Int = _nextIdCounter

  /** @return a deep structural copy of `this` */
  final def deepCopy(
    scope_mapping: mutable.Map[Graph.Scope, Graph.Scope] = mutable.Map()
  ): Graph = {
    val copy = new GraphImpl(
      this.rootScope.deepCopy(scope_mapping),
      this._nextIdCounter
    )
    copy.links       = this.links
    copy.sourceLinks = this.sourceLinks
    copy.targetLinks = this.targetLinks
    copy
  }

  private[analyse] def getLinks(): Set[Graph.Link] = links

  final def freeze(): Unit = {
    _nextIdCounter = -1
  }

  /** Creates a deep copy of the aliasing graph structure.
    *
    * @return a copy of the graph structure
    */
  private[analyse] def copy: Graph = {
    val graph = new GraphImpl(
      rootScope.deepCopy(mutable.Map()),
      _nextIdCounter
    )
    graph.links       = links
    graph.sourceLinks = sourceLinks
    graph.targetLinks = targetLinks

    graph
  }

  /** Determines whether `this` is equal to `obj`.
    *
    * @param obj the object to compare against.
    * @return `true` if `this == obj`, otherwise `false`
    */
  override def equals(obj: Any): Boolean =
    obj match {
      case that: GraphImpl =>
        (this.links == that.links) && (this.rootScope == that.rootScope)
      case _ => false
    }

  /** Generates a new identifier for a node in the graph.
    *
    * @return a unique identifier for this graph
    */
  private[graph] def nextId(scope: ScopeImpl): GraphImpl.Id = {
    val nextId = _nextIdCounter
    if (nextId < 0) {
      throw new IllegalStateException("Cannot emit new IDs. Frozen!")
    }
    _nextIdCounter += 1
    if (scope != null) {
      toScope.put(nextId, scope)
    }
    nextId
  }

  /** Resolves any links for the given usage of a symbol, assuming the symbol
    * is a local variable.
    *
    * @param occurrence the symbol usage
    * @return the link, if it exists
    */
  final def resolveLocalUsage(
    occurrence: GraphOccurrence.Use,
    df: GraphOccurrence.Def
  ): Option[Graph.Link] = {
    Option(occurrence.scope()).flatMap(
      _.asInstanceOf[ScopeImpl].resolveUsage(occurrence, df).map { link =>
        addSourceTargetLink(link)
        links += link
        link
      }
    )
  }

  private def addSourceTargetLink(link: Graph.Link): Unit = {
    // commented out: used from DebugEvalNode
    // org.enso.common.Asserts.assertInJvm(!frozen)
    sourceLinks = sourceLinks.updatedWith(link.source)(v =>
      v.map(s => s + link).orElse(Some(Set(link)))
    )
    targetLinks = targetLinks.updatedWith(link.target)(v =>
      v.map(s => s + link).orElse(Some(Set(link)))
    )
  }

  /** Returns a string representation of the graph.
    *
    * @return a string representation of `this`
    */
  override def toString: String =
    s"Graph(links = $links, rootScope = $rootScope)"

  /** Pretty prints the graph.
    *
    * @return a pretty-printed string representation of the graph
    */
  @unused private def pretty: String = {
    val original = toString
    Debug.pretty(original)
  }

  /** Gets all links in which the provided `id` is a participant.
    *
    * @param id the identifier for the symbol
    * @return a list of links in which `id` occurs
    */
  final def linksFor(id: GraphImpl.Id): Set[Graph.Link] = {
    sourceLinks.getOrElse(id, Set.empty[Graph.Link]) ++ targetLinks
      .getOrElse(
        id,
        Set()
      )
  }

  /** Finds all links in the graph where `symbol` appears in the role
    * specified by `T`.
    *
    * @param symbol the symbol to find links for
    * @tparam T the role in which `symbol` should occur
    * @return a set of all links in which `symbol` occurs with role `T`
    */
  private[analyse] def linksFor[T <: GraphOccurrence: ClassTag](
    symbol: GraphImpl.Symbol
  ): Set[Graph.Link] = {
    val idsForSym = rootScope.symbolToIds[T](symbol)

    links.filter(l =>
      idsForSym.contains(l.source) || idsForSym.contains(l.target)
    )
  }

  /** Obtains the occurrence for a given ID, from whichever scope in which it
    * occurs.
    *
    * @param id the occurrence identifier
    * @return the occurrence for `id`, if it exists
    */
  final def getOccurrence(id: GraphImpl.Id): Option[GraphOccurrence] =
    scopeFor(id).flatMap(_.getOccurrence(id))

  /** Gets the link from an id to the definition of the symbol it represents.
    *
    * @param id the identifier to find the definition link for
    * @return the definition link for `id` if it exists
    */
  final def defLinkFor(id: GraphImpl.Id): Option[Graph.Link] = {
    linksFor(id).find { edge =>
      val occ = getOccurrence(edge.target)
      occ match {
        case Some(GraphOccurrence.Def(_, _, _, _, _)) => true
        case _                                        => false
      }
    }
  }

  /** Gets the scope where a given ID is defined in the graph.
    *
    * @param id the id to find the scope for
    * @return the scope where `id` occurs
    */
  final def scopeFor(id: GraphImpl.Id): Option[ScopeImpl] = {
    val fastOrNull = toScope.get(id)
    if (fastOrNull == null) {
      val slow = rootScope.scopeFor(id)
      if (slow.isDefined) {
        toScope.put(id, slow.orNull)
      }

      slow
    } else {
      Option(fastOrNull)
    }
  }

  /** Finds the scopes in which a name occurs with a given role.
    *
    * @param symbol the symbol
    * @tparam T the role in which `symbol` occurs
    * @return all the scopes where `symbol` occurs with role `T`
    */
  private[analyse] def scopesFor[T <: GraphOccurrence: ClassTag](
    symbol: GraphImpl.Symbol
  ): List[ScopeImpl] = {
    rootScope.scopesForSymbol[T](symbol)
  }

  /** Counts the number of scopes in this scope.
    *
    * @return the number of scopes that are either this scope or children of
    *         it
    */
  private[analyse] def numScopes: Int = {
    rootScope.scopeCount
  }

  /** Determines the maximum nesting depth of scopes through this scope.
    *
    * @return the maximum nesting depth of scopes through this scope.
    */
  private[analyse] def nesting: Int = {
    rootScope.maxNesting
  }

  /** Determines if the provided ID is capable of shadowing other bindings
    *
    * @param id the occurrence identifier
    * @return `true` if `id` shadows other bindings, otherwise `false`
    */
  private[analyse] def canShadow(id: GraphImpl.Id): Boolean = {
    scopeFor(id)
      .flatMap(
        _.getOccurrence(id).flatMap {
          case d: GraphOccurrence.Def => Some(d)
          case _                      => None
        }
      )
      .isDefined
  }

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
  final def knownShadowedDefinitions(
    definition: GraphOccurrence
  ): Set[GraphOccurrence] = {
    def getShadowedIds(
      scope: ScopeImpl
    ): Set[GraphOccurrence] = {
      scope.occurrences.values.collect {
        case d: GraphOccurrence.Def if d.symbol == definition.symbol => d
      } ++ scope.parent.map(getShadowedIds).getOrElse(Set())
    }.toSet

    definition match {
      case d: GraphOccurrence.Def =>
        scopeFor(d.id).flatMap(_.parent) match {
          case Some(scope) => getShadowedIds(scope) // + globals
          case None        => Set()
        }
      case _: GraphOccurrence.Use => Set()
    }
  }

  /** Gets all symbols defined in the graph.
    *
    * @return the set of symbols defined in this graph
    */
  private[analyse] def symbols: Set[GraphImpl.Symbol] = {
    rootScope.symbols
  }

  /** Goes from a symbol to all identifiers that relate to that symbol in
    * the role specified by `T`.
    *
    * @param symbol the symbol to find identifiers for
    * @tparam T the role in which `symbol` should occur
    * @return a list of identifiers for that symbol
    */
  private[analyse] def symbolToIds[T <: GraphOccurrence: ClassTag](
    symbol: GraphImpl.Symbol
  ): List[GraphImpl.Id] = {
    rootScope.symbolToIds[T](symbol)
  }

  /** Goes from an identifier to the associated symbol.
    *
    * @param id the identifier of an occurrence
    * @return the symbol associated with `id`, if it exists
    */
  private[analyse] def idToSymbol(
    id: GraphImpl.Id
  ): Option[GraphImpl.Symbol] = {
    rootScope.idToSymbol(id)
  }
}
object GraphImpl {

  /** The type of symbols on the graph. */
  type Symbol = String

  /** The type of identifiers on the graph. */
  type Id = Int

}
