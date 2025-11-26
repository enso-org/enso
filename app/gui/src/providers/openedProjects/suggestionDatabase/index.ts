import type { ProjectStore } from '$/providers/openedProjects/project'
import type { ProjectNameStore } from '$/providers/openedProjects/projectNames'
import {
  entryIsCallable,
  isUserSelectableType,
  SuggestionKind,
  type CallableSuggestionEntry,
  type MethodSuggestionEntry,
  type SuggestionEntry,
  type SuggestionId,
  type TypeSuggestionEntry,
} from '$/providers/openedProjects/suggestionDatabase/entry'
import { SuggestionUpdateProcessor } from '$/providers/openedProjects/suggestionDatabase/lsUpdate'
import { ExpressionTag } from '@/components/GraphEditor/widgets/WidgetSelection/tags'
import { assert } from '@/util/assert'
import { ReactiveDb, ReactiveIndex } from '@/util/database/reactiveDb'
import type { MethodPointer } from '@/util/methodPointer'
import { AsyncQueue } from '@/util/net'
import { ProjectPath } from '@/util/projectPath'
import type { QualifiedName } from '@/util/qualifiedName'
import { proxyRefs } from '@/util/reactivity'
import * as iter from 'enso-common/src/utilities/data/iter'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { computed, markRaw, readonly, ref } from 'vue'
import { LanguageServer } from 'ydoc-shared/languageServer'
import type { SuggestionDatabaseUpdates } from 'ydoc-shared/languageServerTypes'
import * as lsTypes from 'ydoc-shared/languageServerTypes/suggestions'
import { exponentialBackoff } from 'ydoc-shared/util/net'

/**
 * Suggestion Database.
 *
 * The entries are retrieved (and updated) from engine through the Language Server API. They represent
 * all entities available in current project (from the project and all imported libraries).
 *
 * It is used for code completion/component browser suggestions (thence the name), but also for
 * retrieving information about method/function in widgets, and many more.
 */
export class SuggestionDb extends ReactiveDb<SuggestionId, SuggestionEntry> {
  private readonly pathToId = new ReactiveIndex(this, (id, entry) => [
    [entry.definitionPath.key(), id],
  ])
  readonly childIdToParentId = new ReactiveIndex(this, (id, entry) => {
    const parentAndChild = entry.definitionPath.normalized().splitAtName()
    if (parentAndChild) {
      const [parentPath] = parentAndChild
      const parents = this.pathToId.lookup(parentPath.key())
      return Array.from(parents, (p) => [id, p])
    }
    return []
  })
  readonly conflictingNames = new ReactiveIndex(this, (id, entry) => [[entry.name, id]])
  private readonly suggestionsByKind = new ReactiveIndex(this, (id, entry) => [[entry.kind, id]])
  private readonly constructorFields = new ReactiveIndex(this, (id, entry) => {
    if (entry.kind !== SuggestionKind.Constructor) return []
    const fields = entry.arguments.map((arg) => arg.name)
    const path = entry.memberOf
    const fieldKeys = fields.map((field) => constructorFieldKey(path, field))
    return Array.from(fieldKeys, (key) => [key, id])
  })

  /** Constructor. */
  constructor() {
    super()
  }

  /** Retrieve all suggestions of the given kind stored in the suggestion database. */
  private *getAllEntriesOfKind<K extends SuggestionKind>(
    kind: K,
  ): IterableIterator<SuggestionEntry & { kind: K }> {
    const ids = this.suggestionsByKind.lookup(kind)
    for (const id of ids) {
      const entry = this.get(id)
      assert(entry?.kind === kind)
      yield entry as SuggestionEntry & { kind: K }
    }
  }

  selectableTypes = computed(() => {
    const allTypeEntries = this.getAllEntriesOfKind(SuggestionKind.Type)
    return [...iter.filter(allTypeEntries, isUserSelectableType)]
  })

  /**
   * Retrieve all methods, optionally filtered by the given criteria.
   *
   * PERFORMANCE: This function performs a linear search over all entries. Depending on usage
   * pattern, a `ReactiveIndex` is likely to be more efficient.
   */
  methods(
    filter: {
      /** Whether to include private methods (false by default). */
      includePrivate?: true
      selfType?: ProjectPath | undefined
      memberOf?: ProjectPath | undefined
      /** If provided, includes only methods that pass the predicate. */
      name?: (name: string) => boolean
    } = {},
  ): MethodSuggestionEntry[] {
    const results: MethodSuggestionEntry[] = []
    for (const method of this.getAllEntriesOfKind(SuggestionKind.Method)) {
      if (!filter.includePrivate && method.isPrivate) continue
      if (filter.selfType != null && !filter.selfType.equals(method.selfType)) continue
      if (filter.memberOf != null && !filter.memberOf.equals(method.memberOf)) continue
      if (filter.name != null && !filter.name(method.name)) continue
      results.push(method)
    }
    return results
  }

  dropdownTypeExpressionTags = computed((): ExpressionTag[] => {
    return Array.from(this.selectableTypes.value, (ty) => ExpressionTag.FromEntry(this, ty))
  })

  /** Look up an entry by its path within a project */
  findByProjectPath(projectPath: ProjectPath): SuggestionId | undefined {
    const [id] = this.pathToId.lookup(projectPath.key())
    return id
  }

  /** Get an entry by its path within a project */
  getEntryByProjectPath(projectPath: ProjectPath): SuggestionEntry | undefined {
    const id = this.findByProjectPath(projectPath)
    if (id != null) return this.get(id)
  }

  /** Get entries of given type and all its parent types. */
  getTypeAndItsParentsEntries(path: ProjectPath): Result<TypeSuggestionEntry[]> {
    let next: ProjectPath | undefined = path
    const result = []
    while (next != null) {
      const entry = this.getEntryByProjectPath(next)
      if (entry?.kind !== SuggestionKind.Type) {
        if (result.length > 0) {
          console.error(
            `Suggestion Database inconsitency: parent type of ${result[result.length - 1]?.definedIn.key()} is not a non-type entity ${next.key()}`,
          )
        }
        return Err(`Path ${next.key()} does not resolve to a type`)
      }
      result.push(entry)
      next = entry.parentType
    }
    return Ok(result)
  }

  /** Same as {@link getEntryByProjectPath}, but usable from dev console for debugging. */
  debugGetEntryByProjectNameAndPath(
    projectName: QualifiedName | undefined,
    path: QualifiedName | undefined,
  ): SuggestionEntry | undefined {
    const id = this.findByProjectPath(ProjectPath.create(projectName, path))
    if (id != null) return this.get(id)
  }

  /** Get ID of method/function by MethodPointer structure (received through expression updates). */
  findByMethodPointer(method: MethodPointer): SuggestionId | undefined {
    return this.findByProjectPath(method.definedOnType.append(method.name))
  }

  /** Get entry of method/function by MethodPointer structure (received through expression updates). */
  entryByMethodPointer(method: MethodPointer): CallableSuggestionEntry | undefined {
    const id = this.findByMethodPointer(method)
    if (id == null) return
    const entry = this.get(id)
    return entry && entryIsCallable(entry) ? entry : undefined
  }

  /** Get a list of constructors for `type` that have an argument named `field`. */
  lookupConstructorField(type: ProjectPath, field: string): Set<SuggestionId> {
    return this.constructorFields.lookup(constructorFieldKey(type, field))
  }

  /** Returns the entry's ancestors, starting with its parent. */
  *ancestors(entry: SuggestionEntry): Iterable<ProjectPath> {
    while (entry.kind === SuggestionKind.Type && entry.parentType) {
      yield entry.parentType
      const parent = this.getEntryByProjectPath(entry.parentType)
      if (!parent) break
      entry = parent
    }
  }
}

/** Helper for serializing keys of `constructorFields` index. */
function constructorFieldKey(type: ProjectPath, field: string): string {
  return `${type.key()}#${field}`
}

/**
 * Description of a Component Group.
 *
 * These are groups displayed in the Component Browser. Also, nodes being a call to method from
 * given group will inherit its color.
 */
export interface GroupInfo {
  color?: string
  name: string
  project: QualifiedName
}

class Synchronizer {
  queue: AsyncQueue<{ currentVersion: number }>

  constructor(
    projectStore: ProjectStore,
    public entries: SuggestionDb,
    updateProcessor: Promise<SuggestionUpdateProcessor>,
  ) {
    const lsRpc = projectStore.lsRpcConnection
    const initState = exponentialBackoff(() =>
      lsRpc.acquireCapability('search/receivesSuggestionsDatabaseUpdates', {}),
    ).then(async (capability) => {
      if (!capability.ok) {
        capability.error.log('Will not receive database updates')
      }
      this.#setupUpdateHandler(lsRpc, updateProcessor) // Do not await
      return Synchronizer.loadDatabase(entries, lsRpc, await updateProcessor)
    })

    this.queue = new AsyncQueue(initState)
  }

  static async loadDatabase(
    entries: SuggestionDb,
    lsRpc: LanguageServer,
    updateProcessor: SuggestionUpdateProcessor,
  ): Promise<{ currentVersion: number }> {
    const initialDb = await exponentialBackoff(() => lsRpc.getSuggestionsDatabase())
    if (!initialDb.ok) {
      initialDb.error.log(
        'Cannot load initial suggestion database. Continuing with empty suggestion database',
      )
      return { currentVersion: 0 }
    }
    for (const lsEntry of initialDb.value.entries) {
      const entry = updateProcessor.entryFromLs(lsEntry.suggestion)
      if (!entry.ok) {
        entry.error.log()
        console.error(`Skipping entry ${lsEntry.id}, the suggestion database will be incomplete!`)
      } else {
        entries.set(lsEntry.id, entry.value)
      }
    }
    return { currentVersion: initialDb.value.currentVersion }
  }

  async #setupUpdateHandler(
    lsRpc: LanguageServer,
    updateProcessorPromise: Promise<SuggestionUpdateProcessor>,
  ) {
    // We can get DB updates received through RPC before processor update and loadDatabase call finishes. We have to receive
    // an queue those updates until we are ready to apply them.
    const earlyUpdates: SuggestionDatabaseUpdates[] = []
    const queueEarlyUpdate = lsRpc.on('search/suggestionsDatabaseUpdates', (param) =>
      earlyUpdates.push(param),
    )
    const updateProcessor = await updateProcessorPromise
    lsRpc.off('search/suggestionsDatabaseUpdates', queueEarlyUpdate)

    const processUpdate = lsRpc.on('search/suggestionsDatabaseUpdates', (param) => {
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
          updateProcessor.applyUpdates(this.entries, param.updates)
          return { currentVersion: param.currentVersion }
        }
      })
    })

    // Before an new update is received, apply all queued updates from before initialization.
    earlyUpdates.forEach(processUpdate)
    earlyUpdates.length = 0
  }
}

async function loadGroups(lsRpc: LanguageServer, firstExecution: Promise<unknown>) {
  await firstExecution
  const groups = await exponentialBackoff(() => lsRpc.getComponentGroups())
  if (!groups.ok) {
    if (!lsRpc.isDisposed) {
      groups.error.log('Cannot read component groups. Continuing without groups')
    }
    return []
  }
  return groups.value.componentGroups.map(
    (group): GroupInfo => ({
      name: group.name,
      ...(group.color ? { color: group.color } : {}),
      project: group.library as QualifiedName,
    }),
  )
}

/** {@link useSuggestionDbStore} composable object */
export type SuggestionDbStore = ReturnType<typeof createSuggestionDbStore>
/**
 * A store maintaining suggestions database.
 */
export function createSuggestionDbStore(
  projectStore: ProjectStore,
  projectNames: ProjectNameStore,
) {
  const entries = new SuggestionDb()
  const groups = ref<GroupInfo[]>([])

  const updateProcessor = loadGroups(
    projectStore.lsRpcConnection,
    projectStore.firstExecution,
  ).then((loadedGroups) => {
    groups.value = loadedGroups
    return new SuggestionUpdateProcessor(loadedGroups, projectNames)
  })

  /** Add an entry to the suggestion database. */
  function mockSuggestion(entry: lsTypes.SuggestionEntry) {
    const id = Math.max(...entries.keys()) + 1
    new SuggestionUpdateProcessor([], projectNames).applyUpdates(entries, [
      {
        type: 'Add',
        id,
        suggestion: entry,
      },
    ])
  }

  const _synchronizer = new Synchronizer(projectStore, entries, updateProcessor)
  return proxyRefs({
    entries: markRaw(entries),
    groups: readonly(groups),
    _synchronizer,
    mockSuggestion,
  })
}
