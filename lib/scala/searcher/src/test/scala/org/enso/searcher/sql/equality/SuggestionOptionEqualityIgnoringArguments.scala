package org.enso.searcher.sql.equality

import org.enso.polyglot.Suggestion
import org.scalactic.Equality

object SuggestionOptionEqualityIgnoringArguments
    extends Equality[Option[Suggestion]] {

  /** @inheritdoc */
  override def areEqual(o1: Option[Suggestion], o2: Any): Boolean =
    (o1, o2) match {
      case (Some(a), Some(b: Suggestion)) =>
        SuggestionEqualityIgnoringArguments.areEqual(a, b)
      case (None, None) =>
        true
      case _ =>
        false
    }
}
