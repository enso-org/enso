/**
 * @file
 *
 * Hooks for working with categories.
 * Categories are shortcuts to specific directories in the Cloud, e.g. Team spaces, Recent and Trash.
 */
import ComputerIcon from '#/assets/computer.svg'
import RecentIcon from '#/assets/recent.svg'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import {
  setLocalDirectories,
  useLocalDirectories,
} from '#/layouts/Drive/Categories/persistentState'
import { useLocalRootDirectory } from '#/layouts/Drive/persistentState'
import { useBackends, useText, useUser } from '$/providers/react'
import type { Backend } from 'enso-common/src/services/Backend'
import { BackendType, Path, type DirectoryId } from 'enso-common/src/services/Backend'
import type {
  AnyCategory,
  AnyCloudCategory,
  AnyLocalCategory,
  Category,
  CategoryByType,
  CategoryId,
  CloudCategory,
  LocalCategory,
  LocalDirectoryCategory,
  RecentCategory,
  TeamCategory,
  TrashCategory,
} from 'enso-common/src/services/Backend/Category'
import { isCloudCategory, isLocalCategory } from 'enso-common/src/services/Backend/Category'
import { newDirectoryId } from 'enso-common/src/services/LocalBackend'
import { organizationIdToDirectoryId } from 'enso-common/src/services/RemoteBackend/ids'
import { getFileName } from 'enso-common/src/utilities/file'
import { createContext, useContext } from 'react'
import invariant from 'tiny-invariant'

/** Result of the useCloudCategoryList hook. */
export type CloudCategoryResult = ReturnType<typeof useCloudCategoryList>

/** List of categories in the Cloud. */
function useCloudCategoryList() {
  const user = useUser()
  const { getText } = useText()

  const cloudCategory: CloudCategory = {
    type: 'cloud',
    id: 'cloud',
    label: getText('cloudCategory'),
    icon: 'cloud',
    homeDirectoryId: user.rootDirectoryId,
    canUploadHere: true,
    backend: BackendType.remote,
  }

  const recentCategory: RecentCategory = {
    type: 'recent',
    id: 'recent',
    label: getText('recentCategory'),
    icon: RecentIcon,
    homeDirectoryId: null,
    canUploadHere: false,
    backend: BackendType.remote,
  }

  const trashCategory: TrashCategory = {
    type: 'trash',
    id: 'trash',
    label: getText('trashCategory'),
    icon: 'trash_small',
    homeDirectoryId: organizationIdToDirectoryId(user.organizationId),
    canUploadHere: false,
    backend: BackendType.remote,
  }

  const predefinedCloudCategories: AnyCloudCategory[] = [
    cloudCategory,
    recentCategory,
    trashCategory,
  ]

  const teamCategories = (user.groups ?? []).map<TeamCategory>((group) => ({
    type: 'team',
    id: group.id,
    team: group,
    rootPath: Path(`enso://Teams/${group.name}`),
    homeDirectoryId: group.homeDirectoryId,
    label: getText('teamCategory', group.name),
    icon: 'people',
    canUploadHere: true,
    backend: BackendType.remote,
  }))

  const categories = [...predefinedCloudCategories, ...teamCategories] satisfies AnyCloudCategory[]

  const getCategoryById = useEventCallback(
    (id: CategoryId) => categories.find((category) => category.id === id) ?? null,
  )

  const getCategoriesByType = useEventCallback(
    <T extends Category['type']>(type: T) =>
      // This is safe, because we know that the result will have the correct type.
      // eslint-disable-next-line no-restricted-syntax
      categories.filter((category) => category.type === type) as CategoryByType<T>[],
  )

  const getCategoryByDirectoryId = useEventCallback(
    (directoryId: DirectoryId): AnyCloudCategory | null =>
      categories.find((category) => category.homeDirectoryId === directoryId) ?? null,
  )

  return {
    categories,
    cloudCategory,
    recentCategory,
    trashCategory,
    teamCategories,
    getCategoryById,
    getCategoriesByType,
    isCloudCategory,
    getCategoryByDirectoryId,
  } as const
}

/** Result of the useLocalCategoryList hook. */
export type LocalCategoryResult = ReturnType<typeof useLocalCategoryList>

/** Create a local directory category. */
function createLocalDirectoryCategory(directory: string): LocalDirectoryCategory {
  return {
    type: 'local-directory',
    id: newDirectoryId(Path(directory)),
    rootPath: Path(directory),
    homeDirectoryId: newDirectoryId(Path(directory)),
    label: getFileName(directory),
    icon: 'folder_small',
    canUploadHere: true,
    backend: BackendType.local,
  }
}

/**
 * List of all categories in the LocalBackend.
 * Usually these are the root folder and the list of favorites
 */
function useLocalCategoryList() {
  const { getText } = useText()
  const { localBackend } = useBackends()
  const rootPath = useLocalRootDirectory() ?? localBackend?.rootPath()
  const localDirectories = useLocalDirectories()

  let categories: readonly AnyLocalCategory[] = []

  const addDirectory = useEventCallback((directory: Path) => {
    setLocalDirectories([...localDirectories, directory])

    return createLocalDirectoryCategory(directory)
  })

  const removeDirectory = useEventCallback((directory: DirectoryId) => {
    const category = getCategoryById(directory)

    if (category != null && category.type === 'local-directory') {
      setLocalDirectories(localDirectories.filter((d) => d !== category.rootPath))
    }
  })

  const getCategoryById = useEventCallback(
    (id: CategoryId) => categories.find((category) => category.id === id) ?? null,
  )

  const getCategoryByDirectoryId = useEventCallback(
    (id: DirectoryId): AnyLocalCategory | null =>
      categories.find((category) => category.homeDirectoryId === id) ?? null,
  )

  const getCategoriesByType = useEventCallback(
    <T extends AnyLocalCategory['type']>(type: T) =>
      // This is safe, because we know that the result will have the correct type.
      // eslint-disable-next-line no-restricted-syntax
      categories.filter((category) => category.type === type) as CategoryByType<T>[],
  )

  if (rootPath == null) {
    return {
      // We don't have any categories if localBackend is not available.
      categories: [],
      localCategory: null,
      directories: null,
      // noop if localBackend is not available.
      addDirectory: () => {},
      // noop if localBackend is not available.
      removeDirectory: () => {},
      getCategoryById,
      getCategoriesByType,
      isLocalCategory,
      getCategoryByDirectoryId: () => null,
    }
  }

  const localCategory: LocalCategory = {
    type: 'local',
    id: 'local',
    label: getText('localCategory'),
    icon: ComputerIcon,
    homeDirectoryId: newDirectoryId(rootPath),
    rootPath,
    canUploadHere: true,
    backend: BackendType.local,
  }

  const localDirectoryCategories = localDirectories.map<LocalDirectoryCategory>(
    createLocalDirectoryCategory,
  )

  categories = localBackend == null ? [] : ([localCategory, ...localDirectoryCategories] as const)

  return {
    categories,
    localCategory,
    directories: localDirectoryCategories,
    addDirectory,
    removeDirectory,
    getCategoryById,
    getCategoriesByType,
    isLocalCategory,
    getCategoryByDirectoryId,
  } as const
}

/** Result of the useCategories hook. */
export type CategoriesResult = ReturnType<typeof useCategories>

/** List of all categories. */
export function useCategories() {
  const cloudCategories = useCloudCategoryList()
  const localCategories = useLocalCategoryList()

  const findCategoryById = useEventCallback((id: CategoryId) => {
    return cloudCategories.getCategoryById(id) ?? localCategories.getCategoryById(id)
  })

  const getCategoryByDirectoryId = useEventCallback((id: DirectoryId): AnyCategory | null => {
    return (
      cloudCategories.getCategoryByDirectoryId(id) ?? localCategories.getCategoryByDirectoryId(id)
    )
  })

  return { cloudCategories, localCategories, findCategoryById, getCategoryByDirectoryId }
}

/** Context value for categories. */
export interface CategoriesContextValue {
  readonly cloudCategories: CloudCategoryResult
  readonly localCategories: LocalCategoryResult
  readonly category: Category
  readonly associatedBackend: Backend
}

export const CategoriesContext = createContext<CategoriesContextValue | null>(null)

/** Returns the current category and the associated backend. */
export function useCategory() {
  const { category, associatedBackend } = useCategoriesAPI()

  return { category, associatedBackend }
}

/** An api to interact with categories. */
export function useCategoriesAPI() {
  const context = useContext(CategoriesContext)

  invariant(context != null, 'useCategory must be used within a CategoriesProvider')

  return context
}
