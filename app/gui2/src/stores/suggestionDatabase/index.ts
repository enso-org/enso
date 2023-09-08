import { defineStore } from 'pinia'
import { reactive, ref } from 'vue'
import type { SuggestionEntry, SuggestionId } from './entry'

export interface Group {
  color: string
  name: string
}

export const useSuggestionDbStore = defineStore('suggestionDatabase', () => {
  const entries = reactive(new Map<SuggestionId, SuggestionEntry>())
  const groups = ref<Array<Group>>([
    { color: '#4D9A29', name: 'Data Input' },
    { color: '#B37923', name: 'Input' },
    { color: '#9735B9', name: 'Time' },
  ])
  return { entries, groups }
})
