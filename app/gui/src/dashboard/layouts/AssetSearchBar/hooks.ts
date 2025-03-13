/** @file Hooks for `AssetSearchBar`. */
import { useStore } from '#/hooks/storeHooks'
import { searchbarSuggestionsStore } from './constants'

/** Sets the suggestions. */
export function useSetSuggestions() {
  return useStore(searchbarSuggestionsStore, (state) => state.setSuggestions, {
    unsafeEnableTransition: true,
  })
}
