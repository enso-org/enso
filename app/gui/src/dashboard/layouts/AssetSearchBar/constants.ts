/** @file A */
import type AssetQuery from '#/utilities/AssetQuery'
import { createStore } from 'zustand'

/** A suggested query. */
export interface Suggestion {
  readonly key: string
  readonly render: () => React.ReactNode
  readonly addToQuery: (query: AssetQuery) => AssetQuery
  readonly deleteFromQuery: (query: AssetQuery) => AssetQuery
}

export const searchbarSuggestionsStore = createStore<{
  readonly suggestions: readonly Suggestion[]
  readonly setSuggestions: (suggestions: readonly Suggestion[]) => void
}>((set) => ({
  suggestions: [],
  setSuggestions: (suggestions) => {
    set({ suggestions })
  },
}))
