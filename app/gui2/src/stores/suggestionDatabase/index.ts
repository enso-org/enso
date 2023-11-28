import { useProjectStore } from '@/stores/project'
import { entryQn, type SuggestionEntry, type SuggestionId } from '@/stores/suggestionDatabase/entry'
import { applyUpdates, entryFromLs } from '@/stores/suggestionDatabase/lsUpdate'
import { ReactiveDb, ReactiveIndex } from '@/util/database/reactiveDb'
import { AsyncQueue, rpcWithRetries } from '@/util/net'
import { type Opt } from '@/util/opt'
import { qnJoin, qnParent, tryQualifiedName, type QualifiedName } from '@/util/qualifiedName'
import { defineStore } from 'pinia'
import { LanguageServer } from 'shared/languageServer'
import type { MethodPointer } from 'shared/languageServerTypes'
import { markRaw, ref, type Ref } from 'vue'

export class SuggestionDb extends ReactiveDb<SuggestionId, SuggestionEntry> {
  nameToId = new ReactiveIndex(this, (id, entry) => [[entryQn(entry), id]])
  childIdToParentId = new ReactiveIndex(this, (id, entry) => {
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

  getEntryByQualifiedName(name: QualifiedName): SuggestionEntry | undefined {
    const [id] = this.nameToId.lookup(name)
    if (id) {
      return this.get(id)
    }
  }

  findByMethodPointer(method: MethodPointer): SuggestionId | undefined {
    if (method == null) return
    const moduleName = tryQualifiedName(method.definedOnType)
    const methodName = tryQualifiedName(method.name)
    if (!moduleName.ok || !methodName.ok) return
    const qualifiedName = qnJoin(moduleName.value, methodName.value)
    const [suggestionId] = this.nameToId.lookup(qualifiedName)
    return suggestionId
  }
}

export interface Group {
  color?: string
  name: string
  project: QualifiedName
}

export function groupColorVar(group: Group | undefined): string {
  if (group) {
    const name = group.name.replace(/\s/g, '-')
    return `--group-color-${name}`
  } else {
    return '--group-color-fallback'
  }
}

export function groupColorStyle(group: Group | undefined): string {
  return `var(${groupColorVar(group)})`
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
      this.loadGroups(lsRpc, projectStore.firstExecution)
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
        // There are rare cases where the database is updated twice in quick succession, with the
        // second update containing the same version as the first. In this case, we still need to
        // apply the second set of updates. Skipping it would result in the database then containing
        // references to entries that don't exist. This might be an engine issue, but accepting the
        // second updates seems to be harmless, so we do that.
        if (param.currentVersion == currentVersion) {
          console.log(
            `Received multiple consecutive suggestion database updates with version ${param.currentVersion}`,
          )
        }

        if (param.currentVersion < currentVersion) {
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

  private async loadGroups(lsRpc: LanguageServer, firstExecution: Promise<unknown>) {
    this.queue.pushTask(async ({ currentVersion }) => {
      await firstExecution
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
  return { entries: markRaw(entries), groups, _synchronizer }
})
