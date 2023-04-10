package org.enso.searcher.sql.equality

import org.enso.polyglot.Suggestion
import org.scalactic.Equality

object SuggestionEqualityIgnoringArguments extends Equality[Suggestion] {

  /** @inheritdoc */
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
