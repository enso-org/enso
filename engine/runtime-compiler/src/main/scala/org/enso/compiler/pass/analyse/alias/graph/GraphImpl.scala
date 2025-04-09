package org.enso.compiler
package pass.analyse
package alias.graph

import org.enso.compiler.debug.Debug

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.annotation.unused
import scala.jdk.CollectionConverters._

/** A graph containing aliasing information for a given root scope in Enso. */
sealed private[graph] class GraphImpl(
  private val rootScopeImpl: Graph.Scope = new ScopeImpl(),
  private var _nextIdCounter: Int        = 0,
  _links: Set[Graph.Link]                = null
) extends Graph {
  private var sourceLinks
    : java.util.Map[GraphImpl.Id, java.util.LinkedHashSet[Graph.Link]] =
    new java.util.HashMap()
  private var targetLinks
    : java.util.Map[GraphImpl.Id, java.util.LinkedHashSet[Graph.Link]] =
    new java.util.HashMap()
  private val toScope: java.util.Map[GraphImpl.Id, ScopeImpl] =
    new java.util.HashMap()
  private var links: java.util.Set[Graph.Link] = {
    if (_links == null) {
      new java.util.HashSet()
    } else {
      new java.util.HashSet(_links.asJava)
    }
  }

  final def rootScope: ScopeImpl =
    this.rootScopeImpl.asInstanceOf[ScopeImpl]

  {
    links.forEach(addSourceTargetLink)
  }

  /** @return the next counter value
    */
  private[graph] def nextIdCounter: Int = _nextIdCounter

  private def clone(
    m: java.util.Map[GraphImpl.Id, java.util.LinkedHashSet[Graph.Link]]
  ): java.util.Map[GraphImpl.Id, java.util.LinkedHashSet[Graph.Link]] = {
    val c =
      new java.util.HashMap[GraphImpl.Id, java.util.LinkedHashSet[Graph.Link]]
    val it = m.entrySet().iterator()
    while (it.hasNext()) {
      val e = it.next()
      c.put(e.getKey(), new java.util.LinkedHashSet(e.getValue()))
    }
    c
  }

  /** @return a deep structural copy of `this` */
  final def deepCopy(
    scope_mapping: mutable.Map[Graph.Scope, Graph.Scope] = mutable.Map()
  ): Graph = {
    val copy = new GraphImpl(
      this.rootScope.deepCopy(scope_mapping),
      this._nextIdCounter
    )
    copy.links       = this.links
    copy.sourceLinks = clone(this.sourceLinks)
    copy.targetLinks = clone(this.targetLinks)
    copy
  }

  private[analyse] def getLinks(): Set[Graph.Link] = links.asScala.toSet

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
    graph.sourceLinks = clone(sourceLinks)
    graph.targetLinks = clone(targetLinks)

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
    occurrence: GraphOccurrence.Use
  ): Option[Graph.Link] = {
    Option(occurrence.scope()).flatMap(
      _.asInstanceOf[ScopeImpl].resolveUsage(occurrence).map { link =>
        addSourceTargetLink(link)
        links.add(link)
        link
      }
    )
  }

  private def addSourceTargetLink(link: Graph.Link): Unit = {
    // commented out: used from DebugEvalNode
    // org.enso.common.Asserts.assertInJvm(!frozen)
    var s = sourceLinks.get(link.source)
    if (s == null) {
      s = new java.util.LinkedHashSet[Graph.Link]()
      sourceLinks.put(link.source, s)
    }
    s.add(link)

    var t = targetLinks.get(link.target)
    if (t == null) {
      t = new java.util.LinkedHashSet[Graph.Link]()
      targetLinks.put(link.target, t)
    }
    t.add(link)
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
  final def linksFor(id: GraphImpl.Id): java.util.Set[Graph.Link] = {
    val s = sourceLinks.get(id)
    val t = targetLinks.get(id)
    if (s == null || t == null) {
      if (s != null) {
        java.util.Collections.unmodifiableSet(s)
      } else if (t != null) {
        java.util.Collections.unmodifiableSet(t)
      } else {
        java.util.Collections.emptySet()
      }
    } else {
      val b = new java.util.LinkedHashSet[Graph.Link]
      b.addAll(s)
      b.addAll(t)
      b
    }
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

    links.stream
      .filter(l => idsForSym.contains(l.source) || idsForSym.contains(l.target))
      .toList
      .asScala
      .toSet
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
    val it = linksFor(id).iterator()
    while (it.hasNext()) {
      val edge = it.next()
      val occ  = getOccurrence(edge.target)
      val found = occ match {
        case Some(GraphOccurrence.Def(_, _, _, _, _)) => true
        case _                                        => false
      }
      if (found) {
        return Some(edge)
      }
    }
    None
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
      val withSymbol: Set[GraphOccurrence.Def] =
        scope.getOccurrences(definition.symbol)
      withSymbol ++ scope.parent.map(getShadowedIds).getOrElse(Set())
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
