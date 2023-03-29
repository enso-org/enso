package org.enso.searcher.sql

import org.enso.polyglot.Suggestion
import org.enso.searcher.SuggestionEntry
import org.scalactic.Equality

object SuggestionEqualityIgnoringArguments extends Equality[Suggestion] {
  override def areEqual(a: Suggestion, o: Any): Boolean = {
    o match {
      case b: Suggestion =>
        a.module == b.module &&
        a.name == b.name &&
        a.externalId == b.externalId &&
        a.returnType == b.returnType &&
        a.documentation == b.documentation
      case _ => false
    }
  }
}

object SuggestionEntryEqualityIgnoringArguments
    extends Equality[SuggestionEntry] {
  override def areEqual(a: SuggestionEntry, o: Any): Boolean = {
    o match {
      case b: SuggestionEntry =>
        a.id == b.id &&
        SuggestionEqualityIgnoringArguments.areEqual(a.suggestion, b.suggestion)
      case _ => false
    }
  }
}
