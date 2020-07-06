package org.enso.searcher

/** The entry in the suggestions database.
  *
  * @param id the suggestion id
  * @param suggestion the suggestion
  */
case class SuggestionEntry(id: Long, suggestion: Suggestion)
