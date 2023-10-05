import { defineStore } from 'pinia'
import { reactive, ref } from 'vue'
import { SuggestionDb, defaultGroups, type Group } from '../src/stores/suggestionDatabase'
import mockDb from './mockSuggestions.json' assert { type: 'json' }
export { SuggestionDb, defaultGroups, type Group } from '../src/stores/suggestionDatabase'

export const useSuggestionDbStore = defineStore('suggestionDatabase', () => {
  const entries = reactive(new SuggestionDb(mockDb.map((entry, i) => [i, entry as any])))
  const groups = ref<Group[]>(defaultGroups())

  return { entries, groups, initializeDb() {} }
})
