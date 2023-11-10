import { useProjectStore } from '@/stores/project'
import { entryQn, type SuggestionEntry, type SuggestionId } from '@/stores/suggestionDatabase/entry'
import { applyUpdates, entryFromLs } from '@/stores/suggestionDatabase/lsUpdate'
import { ReactiveDb, ReactiveIndex } from '@/util/database/reactiveDb'
import { AsyncQueue, rpcWithRetries } from '@/util/net'
import { type Opt } from '@/util/opt'
import { qnParent, type QualifiedName } from '@/util/qualifiedName'
import { defineStore } from 'pinia'
import { LanguageServer } from 'shared/languageServer'
import { reactive, ref, type Ref } from 'vue'

export class SuggestionDb {
  _internal = new ReactiveDb<SuggestionId, SuggestionEntry>()
  nameToId = new ReactiveIndex(this._internal, (id, entry) => [[entryQn(entry), id]])
  parent = new ReactiveIndex(this._internal, (id, entry) => {
    let qualifiedName: Opt<QualifiedName>
    if (entry.memberOf) {
      qualifiedName = entry.memberOf
    } else {
      qualifiedName = qnParent(entryQn(entry))
    }
    if (qualifiedName) {
      const parents = Array.from(this.nameToId.lookup(qualifiedName))
      return parents.map((p) => [id, p])
    }
    return []
  })
  set(id: SuggestionId, entry: SuggestionEntry): void {
    this._internal.set(id, reactive(entry))
  }
  get(id: SuggestionId): SuggestionEntry | undefined {
    return this._internal.get(id)
  }
  delete(id: SuggestionId): boolean {
    return this._internal.delete(id)
  }
  entries(): IterableIterator<[SuggestionId, SuggestionEntry]> {
    return this._internal.entries()
  }
}

export interface Group {
  color?: string
  name: string
  project: QualifiedName
}

class Synchronizer {
  queue: AsyncQueue<{ currentVersion: number }>

  constructor(
    public entries: SuggestionDb,
    public groups: Ref<Group[]>,
  ) {
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
    this.queue.pushTask(async ({ currentVersion }) => {
      const groups = await lsRpc.getComponentGroups()
      this.groups.value = groups.componentGroups.map(
        (group): Group => ({
          name: group.name,
          ...(group.color ? { color: group.color } : {}),
          project: group.library as QualifiedName,
        }),
      )
      return { currentVersion }
    })
  }
}

export const useSuggestionDbStore = defineStore('suggestionDatabase', () => {
  const entries = new SuggestionDb()
  const groups = ref<Group[]>([])
  const _synchronizer = new Synchronizer(entries, groups)
  return { entries, groups, _synchronizer }
})
