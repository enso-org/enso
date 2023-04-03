package org.enso.searcher.sql.equality

import org.enso.polyglot.Suggestion
import org.enso.searcher.SuggestionEntry
import org.scalactic.Equality

trait SuggestionsEquality {

  implicit def suggestionEquality: Equality[Suggestion] =
    SuggestionEqualityIgnoringArguments

  implicit def suggestionEntryEquality: Equality[SuggestionEntry] =
    SuggestionEntryEqualityIgnoringArguments

  implicit def suggestionOptionEquality: Equality[Option[Suggestion]] =
    SuggestionOptionEqualityIgnoringArguments
}

object SuggestionsEquality extends SuggestionsEquality
