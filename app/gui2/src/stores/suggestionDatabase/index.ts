import { colorFromString } from '@/util/colors'
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
  lastUpdate: Promise<{ currentVersion: number }>

  constructor(entries: SuggestionDb, groups: Ref<Group[]>) {
    this.entries = entries
    this.groups = groups
    const projectStore = useProjectStore()
    this.lastUpdate = projectStore.lsRpcConnection.then(async (lsRpc) => {
      await lsRpc.acquireCapability('search/receivesSuggestionsDatabaseUpdates', {})
      this.setupUpdateHandler(lsRpc)
      this.setupGroupsFetcher(lsRpc)
      return Synchronizer.loadDatabase(entries, lsRpc, groups.value)
    })
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
      this.lastUpdate = this.lastUpdate.then(async ({ currentVersion }) => {
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

  private setupGroupsFetcher(lsRpc: LanguageServer) {
    lsRpc.once('executionContext/executionComplete', async () => {
      const groups = await lsRpc.getComponentGroups()
      this.groups.value = groups.componentGroups.map(
        (group): Group => ({
          name: group.name,
          color: group.color ?? colorFromString(`${group.library}.${group.name}`),
          project: group.library as QualifiedName,
        }),
      )
    })
  }
}

export const useSuggestionDbStore = defineStore('suggestionDatabase', () => {
  const entries = reactive(new SuggestionDb())
  const groups = ref<Group[]>([])

  const synchronizer = new Synchronizer(entries, groups)
  return { entries, groups, _synchronizer: synchronizer }
})
