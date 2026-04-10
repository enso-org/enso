import { proxyRefs, type ToValue } from '$/utils/reactivity'
import { useZustandStoreRef } from '$/utils/zustand'
import type { Opt } from '@/util/data/opt'
import type { Icon } from '@/util/iconMetadata/iconName'
import { createGlobalState } from '@vueuse/core'
import {
  BackendType,
  isUserGroupId,
  Path,
  UserGroupId,
  type DirectoryId,
  type User,
} from 'enso-common/src/services/Backend'
import { newDirectoryId } from 'enso-common/src/services/LocalBackend'
import { organizationIdToDirectoryId } from 'enso-common/src/services/RemoteBackend/ids'
import { getFileName } from 'enso-common/src/utilities/file'
import { computed, toValue } from 'vue'
import { createStore } from 'zustand'
import { persist } from 'zustand/middleware'
import { useAuth } from './auth'
import { useBackends } from './backends'
import { useLocalPaths } from './localDirectories'
import { useText } from './text'

export interface PredefinedCategory {
  type: 'cloud' | 'recent' | 'trash' | 'local'
}

export interface TeamDirectory {
  type: 'team'
  groupId: UserGroupId
}

export interface LocalDirectory {
  type: 'localDirectory'
  path: Path
}

export type Category = PredefinedCategory | TeamDirectory | LocalDirectory
export type CategoryType = Category['type']

export const CATEGORY_BACKEND: Record<CategoryType, BackendType> = {
  cloud: BackendType.remote,
  recent: BackendType.remote,
  trash: BackendType.remote,
  team: BackendType.remote,
  local: BackendType.local,
  localDirectory: BackendType.local,
}
const TEAM_CATEGORY_KEY_PREFIX = 'team-'
const LOCAL_DIRECTORY_KEY_PREFIX = 'local-'

/** A key string of panel. May be used as Set element or Map key. */
export function categoryKey(category: Category) {
  switch (category.type) {
    case 'cloud':
    case 'recent':
    case 'trash':
    case 'local':
      return category.type
    case 'team':
      return `${TEAM_CATEGORY_KEY_PREFIX}${category.groupId}`
    case 'localDirectory':
      return `${LOCAL_DIRECTORY_KEY_PREFIX}${category.path}`
  }
}

/** Parse key string created by {@link categoryKey}. */
export function categoryFromKey(key: Opt<string>): Category | null {
  switch (key) {
    case 'cloud':
    case 'recent':
    case 'trash':
    case 'local':
      return { type: key }
    default:
      if (key == null) return null
      else if (key.startsWith(TEAM_CATEGORY_KEY_PREFIX)) {
        const groupId = key.substring(TEAM_CATEGORY_KEY_PREFIX.length)
        if (!isUserGroupId(groupId)) return null
        return {
          type: 'team',
          groupId: groupId,
        }
      } else if (key.startsWith(LOCAL_DIRECTORY_KEY_PREFIX)) {
        return {
          type: 'localDirectory',
          path: Path(key.substring(LOCAL_DIRECTORY_KEY_PREFIX.length)),
        }
      } else return null
  }
}

/** Check categories equality. */
export function categoryEq(a: Opt<Category>, b: Opt<Category>) {
  return (a ? categoryKey(a) : '') === (b ? categoryKey(b) : '')
}

/** Check if category uses remote backend. */
export function isCloudCategory(category: Category) {
  return CATEGORY_BACKEND[category.type] === BackendType.remote
}

/** Check if category uses local backend. */
export function isLocalCategory(category: Category) {
  return CATEGORY_BACKEND[category.type] === BackendType.local
}

/** Get icon representing given category type. */
export function categoryIcon(category: CategoryType): Icon {
  switch (category) {
    case 'cloud':
    case 'recent':
      return category
    case 'local':
      return 'system'
    case 'trash':
      return 'trash_small'
    case 'team':
      return 'people'
    case 'localDirectory':
      return 'folder_small'
  }
}

interface LocalRootDirectoryStoreState {
  readonly localDirectories: readonly Path[]
}

const localDirectoryStore = createStore<LocalRootDirectoryStoreState>()(
  persist(
    (): LocalRootDirectoryStoreState => ({
      localDirectories: [],
    }),
    { name: 'enso-local-saved-directories', version: 1 },
  ),
)

/**
 * Store containing information of drive panel categories.
 *
 * Also allows adding and removing local directories.
 */
export type CategoriesStore = ReturnType<typeof createCategoriesStore>

function createCategoriesStore(userData: ToValue<Opt<User>>) {
  const { getText } = useText()
  const backends = useBackends()
  const localPaths = useLocalPaths()

  const teamCategories = computed(
    () =>
      toValue(userData)?.groups?.map(
        (group): TeamDirectory => ({
          type: 'team',
          groupId: group.id,
        }),
      ) ?? [],
  )
  const groupById = computed(
    () => new Map(toValue(userData)?.groups?.map((group) => [group.id, group])),
  )
  const rootPath = computed(
    () => localPaths.localRootDirectory ?? backends.localBackend?.rootPath(),
  )

  const localDirectories = useZustandStoreRef(
    localDirectoryStore,
    (state) => state.localDirectories,
  )

  const localDirectoryCategories = computed(() =>
    localDirectories.value.map((dir): LocalDirectory => ({ type: 'localDirectory', path: dir })),
  )

  const cloudCategoriesList = computed((): Category[] => [
    { type: 'cloud' },
    ...teamCategories.value,
    { type: 'recent' },
    { type: 'trash' },
  ])
  const localCategoriesList = computed((): Category[] => [
    { type: 'local' },
    ...localDirectoryCategories.value,
  ])

  function categoryLabel(category: Category) {
    switch (category.type) {
      case 'cloud':
        return getText('cloudCategory')
      case 'recent':
        return getText('recentCategory')
      case 'trash':
        return getText('trashCategory')
      case 'local':
        return getText('localCategory')
      case 'team':
        return getText('teamCategory', groupById.value.get(category.groupId)?.name ?? 'UNKNOWN')
      case 'localDirectory':
        return getFileName(category.path)
    }
  }

  function categoryDirectoryId(category: Category): DirectoryId | null {
    const user = toValue(userData)
    switch (category.type) {
      case 'cloud':
        return user?.rootDirectoryId ?? null
      case 'recent':
        return null
      case 'trash':
        return user != null ? organizationIdToDirectoryId(user?.organizationId) : null
      case 'local': {
        return rootPath.value != null ? newDirectoryId(rootPath.value) : null
      }
      case 'team':
        return groupById.value.get(category.groupId)?.homeDirectoryId ?? null
      case 'localDirectory':
        return newDirectoryId(category.path)
    }
  }

  function getCategoryByDirectoryId(dirId: DirectoryId) {
    return [...cloudCategoriesList.value, ...localCategoriesList.value].find(
      (category) => categoryDirectoryId(category) === dirId,
    )
  }

  function categoryRootPath(category: Category): Path | null {
    switch (category.type) {
      case 'team': {
        const group = groupById.value.get(category.groupId)
        return group != null ? Path(`enso://Teams/${group.name}`) : null
      }
      case 'local':
        return rootPath.value ?? null
      case 'localDirectory':
        return category.path
      default:
        return null
    }
  }

  function addLocalDirectory(path: Path) {
    const state = localDirectoryStore.getState()
    if (!state.localDirectories.includes(path)) {
      localDirectoryStore.setState({ localDirectories: [...state.localDirectories, path] })
      return true
    }
    return false
  }

  function removeLocalDirectory(path: Path) {
    const state = localDirectoryStore.getState()
    const index = state.localDirectories.findIndex((p) => p === path)
    if (index >= 0) {
      const newList = state.localDirectories.filter((_, i) => i !== index)
      localDirectoryStore.setState({ localDirectories: newList })
      return true
    }
    return false
  }

  return proxyRefs({
    cloudCategoriesList,
    localCategoriesList,
    categoryLabel,
    categoryDirectoryId,
    getCategoryByDirectoryId,
    categoryRootPath,
    addLocalDirectory,
    removeLocalDirectory,
  })
}

export const useCategories = createGlobalState(() => {
  const auth = useAuth()
  return createCategoriesStore(() => auth.session?.user)
})

/**
 * The drop operation to use when transferring assets between categories.
 * @param from - The category to transfer from.
 * @param to - The category to transfer to.
 * @returns The drop operation to use.
 */
export function dropOperationBetweenCategories(
  from: Category,
  to: Category,
  parentId: DirectoryId | null = null,
): 'cancel' | 'copy' | 'move' | undefined {
  // Moving into the same category without a parentId is not allowed.
  if (categoryEq(from, to) && parentId == null) {
    return 'cancel'
  }

  if (to.type === 'recent' || from.type === 'recent') {
    return 'cancel'
  }

  if (isLocalCategory(from)) {
    if (to.type === 'trash') {
      return 'cancel'
    }
  }

  if (isCloudCategory(from) !== isCloudCategory(to)) {
    if (isLocalCategory(from) || isLocalCategory(to)) {
      return 'copy'
    }
  }

  switch (from.type) {
    case 'cloud':
      return 'move'
    case 'team':
      return to.type === 'trash' ? 'move' : 'copy'
    case 'trash':
      return 'move'
    case 'local':
    case 'localDirectory':
      return 'move'
  }
}

/** Whether an asset can be transferred between categories. */
export function canTransferBetweenCategories(
  from: Category,
  to: Category,
  parentId: DirectoryId | null = null,
) {
  const operation = dropOperationBetweenCategories(from, to, parentId)

  return operation !== 'cancel'
}
