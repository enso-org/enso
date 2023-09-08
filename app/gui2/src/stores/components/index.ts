import { computed, ref, type ComputedRef } from 'vue'
import { defineStore } from 'pinia'
import { SuggestionKind, type SuggestionEntry } from '@/stores/suggestionDatabase/entry'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { type Filter, Filtering, type MatchResult, compareMatches } from './filtering'
import { qnIsTopElement, qnLastSegment } from '@/util/qualifiedName'

export interface Component {
  suggestion_id: number
  icon: string
  label: string
  match: MatchResult
  group: number
}

export function labelOfEntry(entry: SuggestionEntry, filtering: Filtering) {
  const isTopModule = entry.kind == SuggestionKind.Module && qnIsTopElement(entry.definedIn)
  if (filtering.isMainView() && isTopModule) return entry.definedIn
  else if (entry.memberOf && entry.selfType == null)
    return `${qnLastSegment(entry.memberOf)}.${entry.name}`
  else return entry.name
}

export const useComponentsStore = defineStore('components', () => {
  const filter = ref<Filter>({})
  const filtering = computed(() => new Filtering(filter.value))
  const suggestionDb = useSuggestionDbStore()
  const components: ComputedRef<Component[]> = computed(() => {
    const currentFiltering = filtering.value
    console.log(currentFiltering)
    const components2: Component[] = Array.from(suggestionDb.entries.entries(), ([id, entry]) => {
      const match = currentFiltering.filter(entry)
      return {
        suggestion_id: id,
        icon: entry.icon_name ?? 'marketplace',
        label: `${labelOfEntry(entry, currentFiltering)} (${match?.score})`,
        match,
        group: entry.group_index,
      }
    })
    components2.sort((a, b) => compareMatches(a.match, b.match))
    return components2
  })

  return { components, filter }
})
