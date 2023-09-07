import { defineStore } from 'pinia'
import { reactive } from 'vue'
import type { SuggestionEntry, SuggestionId } from './entry'

export interface Group {
  color: string
  name: string
}

export const useComponentsStore = defineStore('components', () => {
  const entries = reactive(new Map<SuggestionId, SuggestionEntry>())
  return { entries }
})
