import { proxyRefs, type ToValue } from '$/utils/reactivity'
import { useZustandStoreRef } from '$/utils/zustand'
import type { Opt } from '@/util/data/opt'
import { createGlobalState } from '@vueuse/core'
import type { Path, User, UserGroupId } from 'enso-common/src/services/Backend'
import { getFileName } from 'enso-common/src/utilities/file'
import { computed, toValue } from 'vue'
import { createStore } from 'zustand'
import { persist } from 'zustand/middleware'
import { useAuth } from './auth'
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

export function categoryKey(category: Category) {
  switch (category.type) {
    case 'cloud':
    case 'recent':
    case 'trash':
    case 'local':
      return category.type
    case 'team':
      return `team/${category.groupId}`
    case 'localDirectory':
      return `team/${category.path}`
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

function createCategoriesStore(userData: ToValue<Opt<User>>) {
  const { getText } = useText()
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

  const localDirectories = useZustandStoreRef(
    localDirectoryStore,
    (state) => state.localDirectories,
  )

  const localDirectoryCategories = computed(() =>
    localDirectories.value.map((dir): LocalDirectory => ({ type: 'localDirectory', path: dir })),
  )

  const categoriesList = computed((): Category[] => [
    { type: 'cloud' },
    ...teamCategories.value,
    { type: 'recent' },
    { type: 'trash' },
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

  return proxyRefs({
    categoriesList,
    categoryLabel,
  })
}

export const useCategories = createGlobalState(() => {
  const auth = useAuth()
  return createCategoriesStore(() => auth.session?.user)
})
