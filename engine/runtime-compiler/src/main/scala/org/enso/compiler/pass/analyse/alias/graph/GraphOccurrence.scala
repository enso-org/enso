package org.enso.compiler.pass.analyse.alias.graph

import org.enso.compiler.core.{ExternalID, Identifier}
import org.enso.compiler.pass.analyse.alias.graph.Graph.Id

import java.util.UUID

/** An occurrence of a given symbol in the aliasing graph.
  * Note that this is not present in the metadata attached to the [[org.enso.compiler.core.IR]] elements,
  * but only in the alias [[Graph]].
  */
sealed trait GraphOccurrence extends Serializable {
  val id: Id
  val symbol: Graph.Symbol
}

object GraphOccurrence {

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
    override val symbol: Graph.Symbol,
    identifier: UUID @Identifier,
    externalId: Option[UUID @ExternalID],
    isLazy: Boolean = false
  ) extends GraphOccurrence

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
    override val symbol: Graph.Symbol,
    identifier: UUID @Identifier,
    externalId: Option[UUID @ExternalID]
  ) extends GraphOccurrence

  // TODO [AA] At some point the analysis should make use of these.
  /** Represents a global symbol that has been _asked for_ in the program.
    *
    * @param id the identifier of the name in the graph
    * @param symbol the text of the name
    */
  sealed case class Global(
    override val id: Id,
    override val symbol: Graph.Symbol
  ) extends GraphOccurrence
}
