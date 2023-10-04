import { type SuggestionEntry, type SuggestionId } from '@/stores/suggestionDatabase/entry'
import { defineStore } from 'pinia'
import { reactive, ref } from 'vue'
import mockDb from './mockSuggestions.json' assert { type: 'json' }

export type SuggestionDb = Map<SuggestionId, SuggestionEntry>
export const SuggestionDb = Map<SuggestionId, SuggestionEntry>

export interface Group {
  color: string
  name: string
}

export const useSuggestionDbStore = defineStore('suggestionDatabase', () => {
  const entries = reactive(new SuggestionDb(mockDb.map((entry, i) => [i, entry as any])))
  const groups = ref<Group[]>([
    { color: '#4D9A29', name: 'Input' },
    { color: '#B37923', name: 'Web' },
    { color: '#9735B9', name: 'Parse' },
    { color: '#4D9A29', name: 'Select' },
    { color: '#B37923', name: 'Join' },
    { color: '#9735B9', name: 'Transform' },
    { color: '#4D9A29', name: 'Output' },
  ])

  return { entries, groups, initializeDb() {} }
})
