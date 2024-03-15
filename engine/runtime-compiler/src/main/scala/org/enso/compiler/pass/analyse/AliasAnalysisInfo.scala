package org.enso.compiler.pass.analyse

import org.enso.compiler.pass.IRPass

/** Information about the aliasing state for a given IR node. */
sealed trait AliasAnalysisInfo extends IRPass.IRMetadata {

  /** The aliasing graph. */
  val graph: AliasAnalysisGraph
}
object AliasAnalysisInfo {
  sealed trait Scope extends AliasAnalysisInfo
  object Scope {

    /** Aliasing information for a root scope.
      *
      * A root scope has a 1:1 correspondence with a top-level binding.
      *
      * @param graph the graph containing the alias information for that node
      */
    sealed case class Root(override val graph: AliasAnalysisGraph)
        extends Scope {
      override val metadataName: String = "AliasAnalysisInfo.Scope.Root"

      /** @inheritdoc */
      override def prepareForSerialization(
        compiler: Compiler
      ): Root = this

      /** @inheritdoc */
      override def restoreFromSerialization(
        compiler: Compiler
      ): Option[Root] = Some(this)

      /** @inheritdoc */
      override def duplicate(): Option[IRPass.IRMetadata] = None
    }

    /** Aliasing information about a child scope.
      *
      * @param graph the graph
      * @param scope the child scope in `graph`
      */
    sealed case class Child(
      override val graph: AliasAnalysisGraph,
      scope: AliasAnalysisGraph.Scope
    ) extends Scope {
      override val metadataName: String = "AliasAnalysisInfo.Scope.Child"

      /** @inheritdoc */
      override def prepareForSerialization(
        compiler: Compiler
      ): Child = this

      /** @inheritdoc */
      override def restoreFromSerialization(
        compiler: Compiler
      ): Option[Child] = Some(this)

      /** @inheritdoc */
      override def duplicate(): Option[IRPass.IRMetadata] = None
    }
  }

  /** Aliasing information for a piece of [[IR]] that is contained within a
    * [[Scope]].
    *
    * @param graph the graph in which this IR node can be found
    * @param id the identifier of this IR node in `graph`
    */
  sealed case class Occurrence(
    override val graph: AliasAnalysisGraph,
    id: AliasAnalysisGraph.Id
  ) extends AliasAnalysisInfo {
    override val metadataName: String = "AliasAnalysisInfo.Occurrence"

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
