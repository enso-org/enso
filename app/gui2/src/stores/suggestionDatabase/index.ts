import { AsyncQueue, rpcWithRetries } from '@/util/net'
import { type QualifiedName } from '@/util/qualifiedName'
import { defineStore } from 'pinia'
import { LanguageServer } from 'shared/languageServer'
import { reactive, ref, type Ref } from 'vue'
import { useProjectStore } from '../project'
import { type SuggestionEntry, type SuggestionId } from './entry'
import { applyUpdates, entryFromLs } from './lsUpdate'

export type SuggestionDb = Map<SuggestionId, SuggestionEntry>
export const SuggestionDb = Map<SuggestionId, SuggestionEntry>

export interface Group {
  color?: string
  name: string
  project: QualifiedName
}

class Synchronizer {
  entries: SuggestionDb
  groups: Ref<Group[]>
  queue: AsyncQueue<{ currentVersion: number }>

  constructor(entries: SuggestionDb, groups: Ref<Group[]>) {
    this.entries = entries
    this.groups = groups

    const projectStore = useProjectStore()
    const initState = projectStore.lsRpcConnection.then(async (lsRpc) => {
      await rpcWithRetries(() =>
        lsRpc.acquireCapability('search/receivesSuggestionsDatabaseUpdates', {}),
      )
      this.setupUpdateHandler(lsRpc)
      return Synchronizer.loadDatabase(entries, lsRpc, groups.value)
    })

    this.queue = new AsyncQueue(initState)
  }

  static async loadDatabase(
    entries: SuggestionDb,
    lsRpc: LanguageServer,
    groups: Group[],
  ): Promise<{ currentVersion: number }> {
    const initialDb = await lsRpc.getSuggestionsDatabase()
    for (const lsEntry of initialDb.entries) {
      const entry = entryFromLs(lsEntry.suggestion, groups)
      if (!entry.ok) {
        entry.error.log()
        console.error(`Skipping entry ${lsEntry.id}, the suggestion database will be incomplete!`)
      } else {
        entries.set(lsEntry.id, entry.value)
      }
    }
    return { currentVersion: initialDb.currentVersion }
  }

  private setupUpdateHandler(lsRpc: LanguageServer) {
    lsRpc.on('search/suggestionsDatabaseUpdates', (param) => {
      this.queue.pushTask(async ({ currentVersion }) => {
        if (param.currentVersion <= currentVersion) {
          console.log(
            `Skipping suggestion database update ${param.currentVersion}, because it's already applied`,
          )
          return { currentVersion }
        } else {
          applyUpdates(this.entries, param.updates, this.groups.value)
          return { currentVersion: param.currentVersion }
        }
      })
    })
  }
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

  const synchronizer = new Synchronizer(entries, groups)
  return { entries, groups, _synchronizer: synchronizer }
})
