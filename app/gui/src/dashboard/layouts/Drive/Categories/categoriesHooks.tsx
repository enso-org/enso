/**
 * @file
 *
 * Hooks for working with categories.
 * Categories are shortcuts to specific directories in the Cloud, e.g. Team spaces, Recent and Trash.
 */
import ComputerIcon from '#/assets/computer.svg'
import RecentIcon from '#/assets/recent.svg'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useLocalStorageState } from '#/hooks/localStoreState'
import type Backend from '#/services/Backend'
import { BackendType, Path, type DirectoryId } from '#/services/Backend'
import { newDirectoryId } from '#/services/LocalBackend'
import { organizationIdToDirectoryId } from '#/services/RemoteBackend'
import { getFileName } from '#/utilities/fileInfo'
import LocalStorage from '#/utilities/LocalStorage'
import { useBackends, useText, useUser } from '$/providers/react'
import { createContext, useContext } from 'react'
import invariant from 'tiny-invariant'
import { z } from 'zod'
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
} from './Category'
import { isCloudCategory, isLocalCategory } from './Category'

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly localRootDirectories: z.infer<typeof LOCAL_ROOT_DIRECTORIES_SCHEMA>
  }
}

const LOCAL_ROOT_DIRECTORIES_SCHEMA = z.string().array().readonly()

LocalStorage.registerKey('localRootDirectories', { schema: LOCAL_ROOT_DIRECTORIES_SCHEMA })

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

/**
 * Result of the useLocalCategoryList hook.
 */
export type LocalCategoryResult = ReturnType<typeof useLocalCategoryList>

/**
 * Create a local directory category.
 */
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
  const [localRootDirectory] = useLocalStorageState('localRootDirectory')
  const rootPath = localRootDirectory != null ? Path(localRootDirectory) : localBackend?.rootPath()
  const [localRootDirectories, setLocalRootDirectories] = useLocalStorageState(
    'localRootDirectories',
    [],
  )

  let categories: readonly AnyLocalCategory[] = []

  const addDirectory = useEventCallback((directory: string) => {
    setLocalRootDirectories([...localRootDirectories, directory])

    return createLocalDirectoryCategory(directory)
  })

  const removeDirectory = useEventCallback((directory: DirectoryId) => {
    const category = getCategoryById(directory)

    if (category != null && category.type === 'local-directory') {
      setLocalRootDirectories(localRootDirectories.filter((d) => d !== category.rootPath))
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

  const localDirectories = localRootDirectories.map<LocalDirectoryCategory>(
    createLocalDirectoryCategory,
  )

  categories = localBackend == null ? [] : ([localCategory, ...localDirectories] as const)

  return {
    categories,
    localCategory,
    directories: localDirectories,
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
