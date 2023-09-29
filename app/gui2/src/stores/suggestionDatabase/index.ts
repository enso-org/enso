import { type QualifiedName } from '@/util/qualifiedName'
import { defineStore } from 'pinia'
import { reactive, ref } from 'vue'
import { useProjectStore } from '../project'
import { type SuggestionEntry, type SuggestionId } from './entry'
import { applyUpdates } from './lsUpdate'

export type SuggestionDb = Map<SuggestionId, SuggestionEntry>
export const SuggestionDb = Map<SuggestionId, SuggestionEntry>

export interface Group {
  color?: string
  name: string
  project: QualifiedName
}

export const useSuggestionDbStore = defineStore('suggestionDatabase', () => {
  const entries = reactive(new SuggestionDb())
  const standardBase = 'Standard.Base' as QualifiedName
  const groups = ref<Group[]>([
    { color: '#4D9A29', name: 'Input', project: standardBase },
    { color: '#B37923', name: 'Web', project: standardBase },
    { color: '#9735B9', name: 'Parse', project: standardBase },
    { color: '#4D9A29', name: 'Select', project: standardBase },
    { color: '#B37923', name: 'Join', project: standardBase },
    { color: '#9735B9', name: 'Transform', project: standardBase },
    { color: '#4D9A29', name: 'Output', project: standardBase },
  ])

  async function initializeDb() {
    const projectStore = useProjectStore()
    const lsRpc = await projectStore.lsRpcConnection
    lsRpc.on('search/suggestionsDatabaseUpdates', (updates) => {
      console.log('updates: ', updates)
      applyUpdates(entries, updates.updates, groups.value)
    })
    await lsRpc.acquireCapability('search/receivesSuggestionsDatabaseUpdates', {})
    await lsRpc.getSuggestionsDatabase()
  }
  return { entries, groups, initializeDb }
})
