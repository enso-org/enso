package org.enso.compiler.pass.analyse

import org.enso.compiler.core.{CompilerError, ExternalID, Identifier}
import org.enso.syntax.text.Debug
import org.enso.compiler.pass.analyse.AliasAnalysisGraph.{Occurrence, Scope}

import java.util.UUID
import scala.collection.mutable
import scala.reflect.ClassTag

/** A graph containing aliasing information for a given root scope in Enso. */
sealed class AliasAnalysisGraph extends Serializable {
  var rootScope: AliasAnalysisGraph.Scope = new AliasAnalysisGraph.Scope()
  var links: Set[AliasAnalysisGraph.Link] = Set()
  var nextIdCounter                       = 0

  private var globalSymbols: Map[AliasAnalysisGraph.Symbol, Occurrence.Global] =
    Map()

  /** @return a deep structural copy of `this` */
  def deepCopy(
    scope_mapping: mutable.Map[Scope, Scope] = mutable.Map()
  ): AliasAnalysisGraph = {
    val copy = new AliasAnalysisGraph
    copy.rootScope     = this.rootScope.deepCopy(scope_mapping)
    copy.links         = this.links
    copy.globalSymbols = this.globalSymbols
    copy.nextIdCounter = this.nextIdCounter
    copy
  }

  /** Registers a requested global symbol in the aliasing scope.
    *
    * @param sym the symbol occurrence
    */
  def addGlobalSymbol(sym: Occurrence.Global): Unit = {
    if (!globalSymbols.contains(sym.symbol)) {
      globalSymbols = globalSymbols + (sym.symbol -> sym)
    }
  }

  /** Creates a deep copy of the aliasing graph structure.
    *
    * @return a copy of the graph structure
    */
  def copy: AliasAnalysisGraph = {
    val graph = new AliasAnalysisGraph
    graph.links         = links
    graph.rootScope     = rootScope.deepCopy(mutable.Map())
    graph.nextIdCounter = nextIdCounter

    graph
  }

  /** Determines whether `this` is equal to `obj`.
    *
    * @param obj the object to compare against.
    * @return `true` if `this == obj`, otherwise `false`
    */
  override def equals(obj: Any): Boolean =
    obj match {
      case that: AliasAnalysisGraph =>
        (this.links == that.links) && (this.rootScope == that.rootScope)
      case _ => false
    }

  /** Generates a new identifier for a node in the graph.
    *
    * @return a unique identifier for this graph
    */
  def nextId(): AliasAnalysisGraph.Id = {
    val nextId = nextIdCounter
    nextIdCounter += 1
    nextId
  }

  /** Resolves any links for the given usage of a symbol, assuming the symbol
    * is a local variable.
    *
    * @param occurrence the symbol usage
    * @return the link, if it exists
    */
  def resolveLocalUsage(
    occurrence: AliasAnalysisGraph.Occurrence.Use
  ): Option[AliasAnalysisGraph.Link] = {
    scopeFor(occurrence.id).flatMap(_.resolveUsage(occurrence).map { link =>
      links += link
      link
    })
  }

  /** Resolves any links for the given usage of a symbol, assuming the symbol
    * is global (i.e. method, constructor etc.)
    *
    * @param occurrence the symbol usage
    * @return the link, if it exists
    */
  def resolveGlobalUsage(
    occurrence: AliasAnalysisGraph.Occurrence.Use
  ): Option[AliasAnalysisGraph.Link] = {
    scopeFor(occurrence.id) match {
      case Some(scope) =>
        globalSymbols
          .get(occurrence.symbol)
          .map(g =>
            AliasAnalysisGraph.Link(occurrence.id, scope.scopesToRoot + 1, g.id)
          )
      case None => None
    }
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
  def pprint: String = {
    val original = toString
    Debug.pretty(original)
  }

  /** Gets all links in which the provided `id` is a participant.
    *
    * @param id the identifier for the symbol
    * @return a list of links in which `id` occurs
    */
  def linksFor(id: AliasAnalysisGraph.Id): Set[AliasAnalysisGraph.Link] = {
    links.filter(l => l.source == id || l.target == id)
  }

  /** Finds all links in the graph where `symbol` appears in the role
    * specified by `T`.
    *
    * @param symbol the symbol to find links for
    * @tparam T the role in which `symbol` should occur
    * @return a set of all links in which `symbol` occurs with role `T`
    */
  def linksFor[T <: Occurrence: ClassTag](
    symbol: AliasAnalysisGraph.Symbol
  ): Set[AliasAnalysisGraph.Link] = {
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
  def getOccurrence(id: AliasAnalysisGraph.Id): Option[Occurrence] =
    scopeFor(id).flatMap(_.getOccurrence(id))

  /** Gets the link from an id to the definition of the symbol it represents.
    *
    * @param id the identifier to find the definition link for
    * @return the definition link for `id` if it exists
    */
  def defLinkFor(id: AliasAnalysisGraph.Id): Option[AliasAnalysisGraph.Link] = {
    linksFor(id).find { edge =>
      val occ = getOccurrence(edge.target)
      occ match {
        case Some(Occurrence.Def(_, _, _, _, _)) => true
        case _                                   => false
      }
    }
  }

  /** Gets the scope where a given ID is defined in the graph.
    *
    * @param id the id to find the scope for
    * @return the scope where `id` occurs
    */
  def scopeFor(id: AliasAnalysisGraph.Id): Option[AliasAnalysisGraph.Scope] = {
    rootScope.scopeFor(id)
  }

  /** Finds the scopes in which a name occurs with a given role.
    *
    * @param symbol the symbol
    * @tparam T the role in which `symbol` occurs
    * @return all the scopes where `symbol` occurs with role `T`
    */
  def scopesFor[T <: AliasAnalysisGraph.Occurrence: ClassTag](
    symbol: AliasAnalysisGraph.Symbol
  ): List[AliasAnalysisGraph.Scope] = {
    rootScope.scopesForSymbol[T](symbol)
  }

  /** Counts the number of scopes in this scope.
    *
    * @return the number of scopes that are either this scope or children of
    *         it
    */
  def numScopes: Int = {
    rootScope.scopeCount
  }

  /** Determines the maximum nesting depth of scopes through this scope.
    *
    * @return the maximum nesting depth of scopes through this scope.
    */
  def nesting: Int = {
    rootScope.maxNesting
  }

  /** Determines if the provided ID is capable of shadowing other bindings
    *
    * @param id the occurrence identifier
    * @return `true` if `id` shadows other bindings, otherwise `false`
    */
  def canShadow(id: AliasAnalysisGraph.Id): Boolean = {
    scopeFor(id)
      .flatMap(
        _.getOccurrence(id).flatMap {
          case d: Occurrence.Def => Some(d)
          case _                 => None
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
  def knownShadowedDefinitions(
    definition: Occurrence
  ): Set[AliasAnalysisGraph.Occurrence] = {
    def getShadowedIds(
      scope: AliasAnalysisGraph.Scope
    ): Set[AliasAnalysisGraph.Occurrence] = {
      scope.occurrences.collect {
        case d: Occurrence.Def if d.symbol == definition.symbol    => d
        case g: Occurrence.Global if g.symbol == definition.symbol => g
      } ++ scope.parent.map(getShadowedIds).getOrElse(Set())
    }

    definition match {
      case d: Occurrence.Def =>
        scopeFor(d.id).flatMap(_.parent) match {
          case Some(scope) => getShadowedIds(scope) // + globals
          case None        => Set()
        }
      case _: Occurrence.Global => Set()
      case _: Occurrence.Use    => Set()
    }
  }

  /** Determines if the provided id is linked to a binding that shadows
    * another binding.
    *
    * @param id the identifier to check
    * @return `true` if the definition of the symbol for `id` shadows another
    *        binding for the same symbol, `false`, otherwise
    */
  def linkedToShadowingBinding(id: AliasAnalysisGraph.Id): Boolean = {
    defLinkFor(id).isDefined
  }

  /** Gets all symbols defined in the graph.
    *
    * @return the set of symbols defined in this graph
    */
  def symbols: Set[AliasAnalysisGraph.Symbol] = {
    rootScope.symbols
  }

  /** Goes from a symbol to all identifiers that relate to that symbol in
    * the role specified by `T`.
    *
    * @param symbol the symbol to find identifiers for
    * @tparam T the role in which `symbol` should occur
    * @return a list of identifiers for that symbol
    */
  def symbolToIds[T <: Occurrence: ClassTag](
    symbol: AliasAnalysisGraph.Symbol
  ): List[AliasAnalysisGraph.Id] = {
    rootScope.symbolToIds[T](symbol)
  }

  /** Goes from an identifier to the associated symbol.
    *
    * @param id the identifier of an occurrence
    * @return the symbol associated with `id`, if it exists
    */
  def idToSymbol(
    id: AliasAnalysisGraph.Id
  ): Option[AliasAnalysisGraph.Symbol] = {
    rootScope.idToSymbol(id)
  }
}
object AliasAnalysisGraph {

  /** The type of symbols on the graph. */
  type Symbol = String

  /** The type of identifiers on the graph. */
  type Id = Int

  /** A representation of a local scope in Enso.
    *
    * @param childScopes all scopes that are _direct_ children of `this`
    * @param occurrences all symbol occurrences in `this` scope
    * @param allDefinitions all definitions in this scope, including synthetic ones.
    *                       Note that there may not be a link for all these definitions.
    */
  sealed class Scope(
    var childScopes: List[Scope]             = List(),
    var occurrences: Set[Occurrence]         = Set(),
    var allDefinitions: List[Occurrence.Def] = List()
  ) extends Serializable {

    var parent: Option[Scope] = None

    /** Counts the number of scopes from this scope to the root.
      *
      * This count includes the root scope, but not the current scope.
      *
      * @return the number of scopes from this scope to the root
      */
    def scopesToRoot: Int = {
      parent.flatMap(scope => Some(scope.scopesToRoot + 1)).getOrElse(0)
    }

    /** Sets the parent of the scope.
      *
      * The parent scope must not be redefined.
      *
      * @return this scope with parent scope set
      */
    def withParent(parentScope: Scope): this.type = {
      assert(parent.isEmpty)
      this.parent = Some(parentScope)
      this
    }

    /** Creates a structural copy of this scope, ensuring that replicated
      * scopes are memoised.
      *
      * @return a copy of `this`
      */
    def deepCopy(
      mapping: mutable.Map[Scope, Scope] = mutable.Map()
    ): Scope = {
      mapping.get(this) match {
        case Some(newCorrespondingScope) => newCorrespondingScope
        case None =>
          val childScopeCopies: mutable.ListBuffer[Scope] =
            mutable.ListBuffer()
          this.childScopes.foreach(scope =>
            childScopeCopies += scope.deepCopy(mapping)
          )
          val newScope =
            new Scope(childScopeCopies.toList, occurrences, allDefinitions)
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
        case that: Scope =>
          if (this.childScopes.length == that.childScopes.length) {
            val childScopesEqual =
              this.childScopes.zip(that.childScopes).forall(t => t._1 == t._2)
            val occurrencesEqual = this.occurrences == that.occurrences

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
    def addChild(): Scope = {
      val scope = new Scope()
      scope.parent = Some(this)
      childScopes ::= scope

      scope
    }

    /** Adds the specified symbol occurrence to this scope.
      *
      * @param occurrence the occurrence to add
      */
    def add(occurrence: Occurrence): Unit = {
      occurrences += occurrence
    }

    /** Adds a definition, including a definition with synthetic name, without
      * any links.
      *
      * @param definition The definition to add.
      */
    def addDefinition(definition: Occurrence.Def): Unit = {
      allDefinitions = allDefinitions ++ List(definition)
    }

    /** Finds an occurrence for the provided ID in the current scope, if it
      * exists.
      *
      * @param id the occurrence identifier
      * @return the occurrence for `id`, if it exists
      */
    def getOccurrence(id: AliasAnalysisGraph.Id): Option[Occurrence] = {
      occurrences.find(o => o.id == id)
    }

    /** Finds any occurrences for the provided symbol in the current scope, if
      * it exists.
      *
      * @param symbol the symbol of the occurrence
      * @tparam T the role for the symbol
      * @return the occurrences for `name`, if they exist
      */
    def getOccurrences[T <: Occurrence: ClassTag](
      symbol: AliasAnalysisGraph.Symbol
    ): Set[Occurrence] = {
      occurrences.collect {
        case o: T if o.symbol == symbol => o
      }
    }

    /** Unsafely gets the occurrence for the provided ID in the current scope.
      *
      * Please note that this will crash if the ID is not defined in this
      * scope.
      *
      * @param id the occurrence identifier
      * @return the occurrence for `id`
      */
    def unsafeGetOccurrence(id: AliasAnalysisGraph.Id): Occurrence = {
      getOccurrence(id).get
    }

    /** Checks whether a symbol occurs in a given role in the current scope.
      *
      * @param symbol the symbol to check for
      * @tparam T the role for it to occur in
      * @return `true` if `symbol` occurs in role `T` in this scope, `false`
      *         otherwise
      */
    def hasSymbolOccurrenceAs[T <: Occurrence: ClassTag](
      symbol: AliasAnalysisGraph.Symbol
    ): Boolean = {
      occurrences.collect { case x: T if x.symbol == symbol => x }.nonEmpty
    }

    /** Resolves usages of symbols into links where possible, creating an edge
      * from the usage site to the definition site.
      *
      * @param occurrence the symbol usage
      * @param parentCounter the number of scopes that the link has traversed
      * @return the link from `occurrence` to the definition of that symbol, if it
      *         exists
      */
    def resolveUsage(
      occurrence: AliasAnalysisGraph.Occurrence.Use,
      parentCounter: Int = 0
    ): Option[AliasAnalysisGraph.Link] = {
      val definition = occurrences.find {
        case AliasAnalysisGraph.Occurrence.Def(_, name, _, _, _) =>
          name == occurrence.symbol
        case _ => false
      }

      definition match {
        case None =>
          parent.flatMap(_.resolveUsage(occurrence, parentCounter + 1))
        case Some(target) =>
          Some(AliasAnalysisGraph.Link(occurrence.id, parentCounter, target.id))
      }
    }

    /** Creates a string representation of the scope.
      *
      * @return a string representation of `this`
      */
    override def toString: String =
      s"Scope(occurrences = $occurrences, childScopes = $childScopes)"

    /** Counts the number of scopes in this scope.
      *
      * @return the number of scopes that are either this scope or children of
      *         it
      */
    def scopeCount: Int = {
      childScopes.map(_.scopeCount).sum + 1
    }

    /** Determines the maximum nesting depth of scopes through this scope.
      *
      * @return the maximum nesting depth of scopes through this scope.
      */
    def maxNesting: Int = {
      childScopes.map(_.maxNesting).foldLeft(0)(Math.max) + 1
    }

    /** Gets the scope where a given ID is defined in the graph.
      *
      * @param id the id to find the scope for
      * @return the scope where `id` occurs
      */
    def scopeFor(id: AliasAnalysisGraph.Id): Option[Scope] = {
      val possibleCandidates = occurrences.filter(o => o.id == id)

      if (possibleCandidates.isEmpty) {
        if (childScopes.isEmpty) {
          None
        } else {
          var childCandidate: Scope = null
          val iter                  = childScopes.iterator
          var moreThanOne           = false
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
      } else if (possibleCandidates.size == 1) {
        Some(this)
      } else {
        throw new CompilerError(s"Multiple occurrences found for ID $id.")
      }
    }

    /** Gets the n-th parent of `this` scope.
      *
      * @param n the number of scopes to walk up
      * @return the n-th parent of `this` scope, if present
      */
    def nThParent(n: Int): Option[Scope] = {
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
    def scopesForSymbol[T <: Occurrence: ClassTag](
      symbol: AliasAnalysisGraph.Symbol
    ): List[Scope] = {
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
    def symbols: Set[AliasAnalysisGraph.Symbol] = {
      val symbolsInThis        = occurrences.map(_.symbol)
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
    def symbolToIds[T <: Occurrence: ClassTag](
      symbol: AliasAnalysisGraph.Symbol
    ): List[AliasAnalysisGraph.Id] = {
      val scopes =
        scopesForSymbol[T](symbol).flatMap(_.getOccurrences[T](symbol))
      scopes.map(_.id)
    }

    /** Goes from an identifier to the associated symbol.
      *
      * @param id the identifier of an occurrence
      * @return the symbol associated with `id`, if it exists
      */
    def idToSymbol(
      id: AliasAnalysisGraph.Id
    ): Option[AliasAnalysisGraph.Symbol] = {
      scopeFor(id).flatMap(_.getOccurrence(id)).map(_.symbol)
    }

    /** Checks if `this` scope is a child of the provided `scope`.
      *
      * @param scope the potential parent scope
      * @return `true` if `this` is a child of `scope`, otherwise `false`
      */
    def isChildOf(scope: Scope): Boolean = {
      val isDirectChildOf = scope.childScopes.contains(this)

      val isChildOfChildren = scope.childScopes
        .map(scope => this.isChildOf(scope))
        .foldLeft(false)(_ || _)

      isDirectChildOf || isChildOfChildren
    }
  }

  /** A link in the [[AliasAnalysisGraph]].
    *
    * The source of the link should always be an [[Occurrence.Use]] while the
    * target of the link should always be an [[Occurrence.Def]].
    *
    * @param source the source ID of the link in the graph
    * @param scopeCount the number of scopes that the link traverses
    * @param target the target ID of the link in the graph
    */
  sealed case class Link(source: Id, scopeCount: Int, target: Id)
      extends Serializable

  /** An occurrence of a given symbol in the aliasing graph. */
  sealed trait Occurrence extends Serializable {
    val id: Id
    val symbol: AliasAnalysisGraph.Symbol
  }
  object Occurrence {

    /** The definition of a symbol in the aliasing graph.
      *
      * @param id the identifier of the name in the graph
      * @param symbol the text of the name
      * @param identifier the identifier of the symbol
      * @param externalId the external identifier for the IR node defining
      *                   the symbol
      * @param isLazy whether or not the symbol is defined as lazy
      */
    sealed case class Def(
      override val id: Id,
      override val symbol: AliasAnalysisGraph.Symbol,
      identifier: UUID @Identifier,
      externalId: Option[UUID @ExternalID],
      isLazy: Boolean = false
    ) extends Occurrence

    /** A usage of a symbol in the aliasing graph
      *
      * Name usages _need not_ correspond to name definitions, as dynamic
      * symbol resolution means that a name used at runtime _may not_ be
      * statically visible in the scope.
      *
      * @param id the identifier of the name in the graph
      * @param symbol the text of the name
      * @param identifier the identifier of the symbol
      * @param externalId the external identifier for the IR node defining
      *                   the symbol
      */
    sealed case class Use(
      override val id: Id,
      override val symbol: AliasAnalysisGraph.Symbol,
      identifier: UUID @Identifier,
      externalId: Option[UUID @ExternalID]
    ) extends Occurrence

    // TODO [AA] At some point the analysis should make use of these.
    /** Represents a global symbol that has been _asked for_ in the program.
      *
      * @param id the identifier of the name in the graph
      * @param symbol the text of the name
      */
    sealed case class Global(
      override val id: Id,
      override val symbol: AliasAnalysisGraph.Symbol
    ) extends Occurrence
  }
}
