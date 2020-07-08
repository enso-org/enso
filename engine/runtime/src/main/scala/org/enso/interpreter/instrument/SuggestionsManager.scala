package org.enso.interpreter.instrument

import org.enso.compiler.context.{Changeset, SuggestionBuilder}
import org.enso.compiler.core.IR
import org.enso.polyglot.runtime.Runtime.Api

class SuggestionsManager(builder: SuggestionBuilder) {

  def createUpdates(
    ir: IR.Module,
    @scala.annotation.unused invalidated: Set[Changeset.NodeId]
  ): Seq[Api.SuggestionsDatabaseUpdate] = {
    val suggestionsAdd = builder.build(ir)
    suggestionsAdd.map(Api.SuggestionsDatabaseUpdate.Add)
  }
}
