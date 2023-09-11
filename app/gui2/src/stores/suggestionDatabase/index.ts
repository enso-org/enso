import { defineStore } from 'pinia'
import { reactive, ref } from 'vue'
import { SuggestionKind, type SuggestionEntry, type SuggestionId } from './entry'
import mockDb from './TypesFunctions.json'

export interface Group {
  color: string
  name: string
}

export const useSuggestionDbStore = defineStore('suggestionDatabase', () => {
  const entries = reactive(new Map<SuggestionId, SuggestionEntry>())
  for (const [id, entry] of mockDb.entries()) {
    entries.set(id, {
      kind: entry.methodType == 'ctor' ? SuggestionKind.Constructor : SuggestionKind.Method,
      definedIn: entry.module,
      memberOf: entry.type ?? entry.module,
      selfType: entry.methodType == 'method' ? entry.type : undefined,
      isPrivate: entry.accessor === 'PUBLIC',
      isUnstable: false,
      name: entry.name,
      aliases: entry.aliases,
      arguments: [],
      returnType: entry.returnType,
      documentation: '',
      iconName: entry.icon,
    })
  }
  const groups = ref<Array<Group>>([
    { color: '#4D9A29', name: 'Data Input' },
    { color: '#B37923', name: 'Input' },
    { color: '#9735B9', name: 'Time' },
  ])
  return { entries, groups }
})
