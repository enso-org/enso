import { type QualifiedName } from '@/util/qualifiedName'
import { defineStore } from 'pinia'
import { reactive, ref } from 'vue'
import { SuggestionDb, type Group } from '../src/stores/suggestionDatabase'
import mockDb from './mockSuggestions.json' assert { type: 'json' }
export { SuggestionDb, type Group } from '../src/stores/suggestionDatabase'

const standardBase = 'Standard.Base' as QualifiedName

// This should be a function, to ensure a deep copy is created every time.
// This shoul
export function placeholderGroups(): Group[] {
  return [
    { color: '#4D9A29', name: 'Input', project: standardBase },
    { color: '#B37923', name: 'Web', project: standardBase },
    { color: '#9735B9', name: 'Parse', project: standardBase },
    { color: '#4D9A29', name: 'Select', project: standardBase },
    { color: '#B37923', name: 'Join', project: standardBase },
    { color: '#9735B9', name: 'Transform', project: standardBase },
    { color: '#4D9A29', name: 'Output', project: standardBase },
  ]
}

export const useSuggestionDbStore = defineStore('suggestionDatabase', () => {
  const entries = reactive(new SuggestionDb(mockDb.map((entry, i) => [i, entry as any])))
  const groups = ref<Group[]>(placeholderGroups())

  return { entries, groups, initializeDb() {} }
})
