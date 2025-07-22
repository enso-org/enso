package org.enso.compiler
package pass.analyse
package alias.graph

import org.enso.compiler.core.CompilerError

import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.reflect.ClassTag

/** A representation of a local scope in Enso.
  *
  * @param childScopes all scopes that are _direct_ children of `this`
  * @param occurrences all symbol occurrences in `this` scope indexed by the identifier of the name
  * @param allDefinitions all definitions in this scope, including synthetic ones.
  *                       Note that there may not be a link for all these definitions.
  */
sealed private[graph] class ScopeImpl(
  private[graph] var _childScopes: List[ScopeImpl] = List(),
  _occurs: java.util.Map[GraphImpl.Id, GraphOccurrence] =
    new java.util.HashMap(),
  _defs: java.util.Collection[GraphOccurrence.Def] = null
) extends Graph.Scope {
  private[graph] val _allDefinitions: java.util.List[
    GraphOccurrence.Def
  ] =
    if (_defs == null) new java.util.ArrayList()
    else new java.util.ArrayList(_defs.stream.map(_.withScope(this)).toList)

  private[graph] var _parent: ScopeImpl = null
  private val occurrencesById: java.util.Map[GraphImpl.Id, GraphOccurrence] =
    _occurs
  private val defsBySymbol
    : java.util.Map[GraphImpl.Symbol, GraphOccurrence.Def] = {
    val bySymbol =
      new java.util.HashMap[GraphImpl.Symbol, GraphOccurrence.Def]()
    allOccurrences().stream
      .filter(_.isInstanceOf[GraphOccurrence.Def])
      .forEach(d => {
        bySymbol.put(d.symbol, d.asInstanceOf[GraphOccurrence.Def])
      })
    bySymbol
  }

  def childScopes = _childScopes

  def forEachOccurenceDefinition(fn: (GraphOccurrence.Def => Unit)): Unit = {
    allOccurrences().forEach {
      case x: GraphOccurrence.Def => fn(x)
      case _                      =>
    }
  }

  private[graph] def writeObject(
    out: org.enso.persist.Persistance.Output
  ): Unit = {
    out.writeInline(classOf[scala.collection.immutable.List[_]], childScopes);
    out.writeObject(allOccurrences().asScala.toSet);
    out.writeInline(classOf[java.util.List[_]], _allDefinitions);
  }

  private def allOccurrences(): java.util.Collection[GraphOccurrence] =
    occurrencesById.values

  def allDefinitions =
    java.util.Collections.unmodifiableCollection(_allDefinitions)
  def parent: Option[ScopeImpl] =
    if (this._parent eq null) None else Some(_parent)

  /** Counts the number of scopes from this scope to the root.
    *
    * This count includes the root scope, but not the current scope.
    *
    * @return the number of scopes from this scope to the root
    */
  private[analyse] def scopesToRoot: Int = {
    parent.flatMap(scope => Some(scope.scopesToRoot + 1)).getOrElse(0)
  }

  /** Sets the parent of the scope.
    *
    * The parent scope must not be redefined.
    *
    * @return this scope with parent scope set
    */
  final def withParent(parentScope: Graph.Scope): this.type = {
    if (this._parent ne parentScope) {
      org.enso.common.Asserts.assertInJvm(parent.isEmpty)
      this._parent = parentScope.asInstanceOf[ScopeImpl]
    }
    this
  }

  /** Creates a structural copy of this scope, ensuring that replicated
    * scopes are memoised.
    *
    * @return a copy of `this`
    */
  final def deepCopy(
    mapping: mutable.Map[Graph.Scope, Graph.Scope] = mutable.Map()
  ): Graph.Scope = {
    mapping.get(this) match {
      case Some(newCorrespondingScope) => newCorrespondingScope
      case None =>
        val childScopeCopies: mutable.ListBuffer[ScopeImpl] =
          mutable.ListBuffer()
        this.childScopes.foreach(scope =>
          childScopeCopies += scope
            .deepCopy(mapping)
            .asInstanceOf[ScopeImpl]
        )
        val newScope =
          new ScopeImpl(
            childScopeCopies.toList,
            new java.util.HashMap(occurrencesById),
            new java.util.ArrayList(_allDefinitions)
          )
        mapping.put(this, newScope)
        newScope
    }
  }

  /** Checks whether `this` is equal to `obj`.
    *
    * @param obj the object to compare `this` against
    * @return `true` if `this == obj`, otherwise `false`
    */
  override def equals(obj: Any): Boolean =
    obj match {
      case that: ScopeImpl =>
        if (this.childScopes.length == that.childScopes.length) {
          val childScopesEqual =
            this.childScopes.zip(that.childScopes).forall(t => t._1 == t._2)
          val occurrencesEqual = this.occurrencesById == that.occurrencesById

          childScopesEqual && occurrencesEqual
        } else {
          false
        }
      case _ => false
    }

  /** Creates and returns a scope that is a child of this one.
    *
    * @return a scope that is a child of `this`
    */
  private[graph] def addChild(): ScopeImpl = {
    val scope = new ScopeImpl()
    scope._parent = this
    _childScopes ::= scope

    scope
  }

  /** Adds the specified symbol occurrence to this scope.
    *
    * @param occurrence the occurrence to add
    */
  private[graph] def add(occurrence: GraphOccurrence): Unit = {
    if (occurrencesById.containsKey(occurrence.id)) {
      throw new CompilerError(
        s"Multiple occurrences found for ID ${occurrence.id}."
      )
    } else {
      occurrencesById.put(occurrence.id, occurrence)
      occurrence match {
        case d: GraphOccurrence.Def =>
          defsBySymbol.put(occurrence.symbol, d)
        case _ =>
      }
    }
  }

  /** Adds a definition, including a definition with synthetic name, without
    * any links.
    *
    * @param definition The definition to add.
    */
  private[graph] def addDefinition(definition: GraphOccurrence.Def): Unit = {
    _allDefinitions.add(definition)
  }

  /** Finds an occurrence for the provided ID in the current scope, if it
    * exists.
    *
    * @param id the occurrence identifier
    * @return the occurrence for `id`, if it exists
    */
  private[analyse] def getOccurrence(
    id: GraphImpl.Id
  ): Option[GraphOccurrence] = {
    Option(occurrencesById.get(id))
  }

  /** Finds any occurrences for the provided symbol in the current scope, if
    * it exists.
    *
    * @param symbol the symbol of the occurrence
    * @tparam T the role for the symbol
    * @return the occurrences for `name`, if they exist
    */
  private[graph] def getOccurrences[T <: GraphOccurrence: ClassTag](
    symbol: GraphImpl.Symbol
  ): Set[T] = {
    allOccurrences().stream
      .filter {
        case o: T if o.symbol == symbol => true
        case _                          => false
      }
      .toList
      .asScala
      .map(_.asInstanceOf[T])
      .toSet
  }

  /** Checks whether a symbol occurs in a given role in the current scope.
    *
    * @param symbol the symbol to check for
    * @tparam T the role for it to occur in
    * @return `true` if `symbol` occurs in role `T` in this scope, `false`
    *         otherwise
    */
  final def hasSymbolOccurrenceAs[T <: GraphOccurrence: ClassTag](
    symbol: GraphImpl.Symbol
  ): Boolean = {
    getOccurrences(symbol).nonEmpty
  }

  /** Resolves usages of symbols into links where possible, creating an edge
    * from the usage site to the definition site.
    *
    * @param occurrence the symbol usage
    * @param parentCounter the number of scopes that the link has traversed
    * @return the link from `occurrence` to the definition of that symbol, if it
    *         exists
    */
  private[analyse] def resolveUsage(
    occurrence: GraphOccurrence.Use,
    parentCounter: Int = 0
  ): Option[Graph.Link] = {
    val definition = Option(defsBySymbol.get(occurrence.symbol))
    definition match {
      case None =>
        parent.flatMap(_.resolveUsage(occurrence, parentCounter + 1))
      case Some(target) =>
        Some(Graph.Link(occurrence.id, parentCounter, target.id()))
    }
  }

  /** Creates a string representation of the scope.
    *
    * @return a string representation of `this`
    */
  override def toString: String =
    s"Scope(occurrences = ${allOccurrences()}, childScopes = $childScopes)"

  /** Counts the number of scopes in this scope.
    *
    * @return the number of scopes that are either this scope or children of
    *         it
    */
  private[analyse] def scopeCount: Int = {
    childScopes.map(_.scopeCount).sum + 1
  }

  /** Determines the maximum nesting depth of scopes through this scope.
    *
    * @return the maximum nesting depth of scopes through this scope.
    */
  private[analyse] def maxNesting: Int = {
    childScopes.map(_.maxNesting).foldLeft(0)(Math.max) + 1
  }

  /** Gets the scope where a given ID is defined in the graph.
    *
    * @param id the id to find the scope for
    * @return the scope where `id` occurs
    */
  private[analyse] def scopeFor(id: GraphImpl.Id): Option[ScopeImpl] = {
    if (!occurrencesById.containsKey(id)) {
      if (childScopes.isEmpty) {
        None
      } else {
        var childCandidate: ScopeImpl = null
        val iter                      = childScopes.iterator
        var moreThanOne               = false
        while (iter.hasNext && !moreThanOne) {
          iter.next().scopeFor(id) match {
            case Some(s) =>
              if (childCandidate == null) {
                childCandidate = s
              } else {
                moreThanOne = true
              }
            case None =>
          }
        }

        if (childCandidate == null) {
          None
        } else if (moreThanOne) {
          throw new CompilerError(s"ID $id defined in multiple scopes.")
        } else {
          Some(childCandidate)
        }
      }
    } else {
      Some(this)
    }
  }

  /** Gets the n-th parent of `this` scope.
    *
    * @param n the number of scopes to walk up
    * @return the n-th parent of `this` scope, if present
    */
  private[analyse] def nThParent(n: Int): Option[ScopeImpl] = {
    if (n == 0) Some(this) else this.parent.flatMap(_.nThParent(n - 1))
  }

  /** Finds the scopes in which a symbol occurs with a given role.
    *
    * Users of this function _must_ explicitly specify `T`, otherwise the
    * results will be an empty list.
    *
    * @param symbol the symbol
    * @tparam T the role in which `name` occurs
    * @return all the scopes where `name` occurs with role `T`
    */
  private[analyse] def scopesForSymbol[T <: GraphOccurrence: ClassTag](
    symbol: GraphImpl.Symbol
  ): List[ScopeImpl] = {
    val occursInThisScope = hasSymbolOccurrenceAs[T](symbol)

    val occurrencesInChildScopes =
      childScopes.flatMap(_.scopesForSymbol[T](symbol))

    if (occursInThisScope) {
      this +: occurrencesInChildScopes
    } else {
      occurrencesInChildScopes
    }
  }

  /** Gets the set of all symbols in this scope and its children.
    *
    * @return the set of symbols
    */
  private[analyse] def symbols: Set[GraphImpl.Symbol] = {
    val symbolsInThis =
      allOccurrences().stream.map(_.symbol).toList.asScala.toSet
    val symbolsInChildScopes = childScopes.flatMap(_.symbols)

    symbolsInThis ++ symbolsInChildScopes
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
    val scopes =
      scopesForSymbol[T](symbol).flatMap(_.getOccurrences[T](symbol))
    scopes.map(_.id)
  }

  /** Goes from an identifier to the associated symbol.
    *
    * @param id the identifier of an occurrence
    * @return the symbol associated with `id`, if it exists
    */
  private[analyse] def idToSymbol(
    id: GraphImpl.Id
  ): Option[GraphImpl.Symbol] = {
    scopeFor(id).flatMap(_.getOccurrence(id)).map(_.symbol)
  }

  /** Checks if `this` scope is a child of the provided `scope`.
    *
    * @param scope the potential parent scope
    * @return `true` if `this` is a child of `scope`, otherwise `false`
    */
  private[analyse] def isChildOf(scope: ScopeImpl): Boolean = {
    val isDirectChildOf = scope.childScopes.contains(this)

    val isChildOfChildren = scope.childScopes
      .map(scope => this.isChildOf(scope))
      .foldLeft(false)(_ || _)

    isDirectChildOf || isChildOfChildren
  }

  private def removeScopeFromParent(scope: ScopeImpl): Unit = {
    _childScopes = childScopes.filter(_ != scope)
  }

  /** Disassociates this Scope from its parent.
    */
  private[compiler] def removeScopeFromParent(): Unit = {
    org.enso.common.Asserts.assertInJvm(this.parent.nonEmpty)
    this.parent.foreach(_.removeScopeFromParent(this))
  }
}
