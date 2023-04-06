package org.enso.searcher.sql.equality

import org.enso.searcher.SuggestionEntry
import org.scalactic.Equality

object SuggestionEntryEqualityIgnoringArguments
    extends Equality[SuggestionEntry] {

  /** @inheritdoc */
  override def areEqual(a: SuggestionEntry, o: Any): Boolean = {
    o match {
      case b: SuggestionEntry =>
        a.id == b.id &&
        SuggestionEqualityIgnoringArguments.areEqual(a.suggestion, b.suggestion)
      case _ => false
    }
  }
}
