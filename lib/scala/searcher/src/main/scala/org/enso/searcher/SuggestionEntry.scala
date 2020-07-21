package org.enso.searcher

import org.enso.polyglot.Suggestion

/** The entry in the suggestions database.
  *
  * @param id the suggestion id
  * @param suggestion the suggestion
  */
case class SuggestionEntry(id: Long, suggestion: Suggestion)
