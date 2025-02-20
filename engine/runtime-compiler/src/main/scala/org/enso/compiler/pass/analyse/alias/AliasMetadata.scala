package org.enso.compiler.pass.analyse.alias

import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.alias.graph.Graph
import org.enso.compiler.pass.analyse.alias.graph.GraphBuilder

/** Information about the aliasing state for a given IR node. */
sealed trait AliasMetadata extends IRPass.IRMetadata {

  /** The aliasing graph. */
  val graph: Graph
}

object AliasMetadata {
  sealed trait Scope extends AliasMetadata

  /** Aliasing information for a root scope.
    *
    * A root scope has a 1:1 correspondence with a top-level binding.
    *
    * @param graph the graph containing the alias information for that node
    */
  sealed case class RootScope(override val graph: Graph) extends Scope {
    override val metadataName: String = "AliasMetadata.RootScope"

    /** @inheritdoc */
    override def prepareForSerialization(
      compiler: Compiler
    ): RootScope = this

    /** @inheritdoc */
    override def restoreFromSerialization(
      compiler: Compiler
    ): Option[RootScope] = Some(this)

    /** @inheritdoc */
    override def duplicate(): Option[IRPass.IRMetadata] = None
  }

  /** Aliasing information about a child scope.
    *
    * @param graph the graph
    * @param scope the child scope in `graph`
    */
  sealed case class ChildScope(
    override val graph: Graph,
    scope: Graph.Scope
  ) extends Scope {
    def this(b: GraphBuilder) = {
      this(b.toGraph(), b.toScope())
    }

    override val metadataName: String = "AliasMetadata.ChildScope"

    /** @inheritdoc */
    override def prepareForSerialization(
      compiler: Compiler
    ): ChildScope = this

    /** @inheritdoc */
    override def restoreFromSerialization(
      compiler: Compiler
    ): Option[ChildScope] = Some(this)

    /** @inheritdoc */
    override def duplicate(): Option[IRPass.IRMetadata] = None
  }

  /** Aliasing information for a piece of [[IR]] that is contained within a
    * [[Scope]].
    *
    * @param graph the graph in which this IR node can be found
    * @param id the identifier of this IR node in `graph`
    */
  sealed case class Occurrence(
    override val graph: Graph,
    id: Graph.Id
  ) extends AliasMetadata {
    override val metadataName: String = "AliasMetadata.Occurrence"

    /** @inheritdoc */
    override def prepareForSerialization(
      compiler: Compiler
    ): Occurrence = this

    /** @inheritdoc */
    override def restoreFromSerialization(
      compiler: Compiler
    ): Option[Occurrence] = Some(this)

    /** @inheritdoc */
    override def duplicate(): Option[IRPass.IRMetadata] = None
  }
}
