import { AsyncQueue, rpcWithRetries } from '@/util/net'
import { type QualifiedName } from '@/util/qualifiedName'
import * as map from 'lib0/map'
import { defineStore } from 'pinia'
import { LanguageServer } from 'shared/languageServer'
import { reactive, ref, watch, type Ref, watchEffect } from 'vue'
import { useProjectStore } from '../project'
import { type SuggestionEntry, type SuggestionId, entryQn } from './entry'
import { applyUpdates, entryFromLs } from './lsUpdate'
import * as map from 'lib0/map'

export class SuggestionDb {
  entries: Map<SuggestionId, SuggestionEntry>
  nameToId: Map<QualifiedName, SuggestionId>
  children: Map<SuggestionId, Set<SuggestionId>>
  constructor() {
    this.entries = reactive(new Map())
    this.nameToId = reactive(new Map())
    watch(this.entries, (entries) => {
      const id = magic()
      const close = watch(() => this.getParent(id), (newParent, _, onCleanup) => {
        const parentId = newParent
        const children = this.children.get(parentId) ?? new Set()
        const setOfChildren = map.setIfUndefined(this.children, parentId, () => new Set())
        setOfChildren.add(id)
        onCleanup(() => {
          setOfChildren.delete(id)
        })
      })
    })
  }
  getParent(id: SuggestionId): SuggestionId | undefined {
    const entry = this.get(id)
    if (!entry) {
      return undefined
    }
    const parent = entry.parent
    if (!parent) {
      return undefined
    }
    return this.nameToId.get(parent)
  }
  set(id: SuggestionId, entry: SuggestionEntry) {
    this.entries.set(id, entry)
    this.nameToId.set(entryQn(entry), id)
  }
  get(id: SuggestionId): SuggestionEntry | undefined {
    return this.entries.get(id)
  }
  delete(id: SuggestionId) {
    const entry = this.get(id)
    if (entry) {
      this.nameToId.delete(entryQn(entry))
    }
    this.entries.delete(id)
  }
}

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
    lsRpc.once('executionContext/executionComplete', async () => {
      const groups = await lsRpc.getComponentGroups()
      this.groups.value = groups.componentGroups.map(
        (group): Group => ({
          name: group.name,
          ...(group.color ? { color: group.color } : {}),
          project: group.library as QualifiedName,
        }),
      )
    })
  }
}

export const useSuggestionDbStore = defineStore('suggestionDatabase', () => {
  const entries = new SuggestionDb()
  const groups = ref<Group[]>([])
  const methodPointerToEntry = reactive(new Map<string, Map<string, SuggestionEntry>>())

  // FIXME: Replace this inefficient watcher with reactive index, once we have it developed.
  watchEffect(() => {
    methodPointerToEntry.clear()
    for (const entry of entries.values()) {
      const methodNameToEntry = map.setIfUndefined(
        methodPointerToEntry,
        entry.definedIn as string,
        () => new Map<string, SuggestionEntry>(),
      )
      methodNameToEntry.set(entry.name, entry)
    }
  })

  const synchronizer = new Synchronizer(entries, groups)
  return { entries, groups, methodPointerToEntry, _synchronizer: synchronizer }
})
