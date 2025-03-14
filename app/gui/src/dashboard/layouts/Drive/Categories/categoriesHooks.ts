/**
 * @file
 *
 * Hooks for working with categories.
 * Categories are shortcuts to specific directories in the Cloud, e.g. team spaces, recent and trash
 * It's not the same as the categories like LocalBackend
 */
import CloudIcon from '#/assets/cloud.svg'
import ComputerIcon from '#/assets/computer.svg'
import FolderFilledIcon from '#/assets/folder_filled.svg'
import PeopleIcon from '#/assets/people.svg'
import RecentIcon from '#/assets/recent.svg'
import Trash2Icon from '#/assets/trash2.svg'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { CategoriesContext } from '#/layouts/Drive/Categories/constants'
import { useUser } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useLocalStorageState } from '#/providers/LocalStorageProvider'
import { useText } from '#/providers/TextProvider'
import { Path, type DirectoryId } from '#/services/Backend'
import { newDirectoryId } from '#/services/LocalBackend'
import { userIdToDirectoryId } from '#/services/RemoteBackend'
import { getFileName } from '#/utilities/fileInfo'
import { LocalStorage } from '#/utilities/LocalStorage'
import { useContext } from 'react'
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
export function useCloudCategoryList() {
  const user = useUser()
  const { getText } = useText()

  const { userId } = user

  const cloudCategory: CloudCategory = {
    type: 'cloud',
    id: 'cloud',
    label: getText('cloudCategory'),
    icon: CloudIcon,
    homeDirectoryId: userIdToDirectoryId(userId),
  }

  const recentCategory: RecentCategory = {
    type: 'recent',
    id: 'recent',
    label: getText('recentCategory'),
    icon: RecentIcon,
  }

  const trashCategory: TrashCategory = {
    type: 'trash',
    id: 'trash',
    label: getText('trashCategory'),
    icon: Trash2Icon,
  }

  const predefinedCloudCategories: AnyCloudCategory[] = [
    cloudCategory,
    recentCategory,
    trashCategory,
  ]

  const teamCategories =
    user.groups?.map<TeamCategory>((group) => ({
      type: 'team',
      id: group.id,
      team: group,
      rootPath: Path(`enso://Teams/${group.name}`),
      homeDirectoryId: group.homeDirectoryId,
      label: getText('teamCategory', group.name),
      icon: PeopleIcon,
    })) ?? []

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
      categories.find((category) => {
        if ('homeDirectoryId' in category) {
          return category.homeDirectoryId === directoryId
        }

        return false
      }) ?? null,
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
 * List of all categories in the LocalBackend.
 * Usually these are the root folder and the list of favorites
 */
export function useLocalCategoryList() {
  const { getText } = useText()
  const localBackend = useLocalBackend()

  const localCategory: LocalCategory = {
    type: 'local',
    id: 'local',
    label: getText('localCategory'),
    icon: ComputerIcon,
    homeDirectoryId: newDirectoryId(localBackend?.rootPath() ?? Path('')),
    rootPath: localBackend?.rootPath() ?? Path(''),
  }

  const predefinedLocalCategories: AnyLocalCategory[] = [localCategory]

  const [localRootDirectories, setLocalRootDirectories] = useLocalStorageState(
    'localRootDirectories',
    [],
  )

  const localCategories = localRootDirectories.map<LocalDirectoryCategory>((directory) => ({
    type: 'local-directory',
    id: newDirectoryId(Path(directory)),
    rootPath: Path(directory),
    homeDirectoryId: newDirectoryId(Path(directory)),
    label: getFileName(directory),
    icon: FolderFilledIcon,
  }))

  const categories =
    localBackend == null ? [] : ([...predefinedLocalCategories, ...localCategories] as const)

  const addDirectory = useEventCallback((directory: string) => {
    setLocalRootDirectories([...localRootDirectories, directory])
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

  const getCategoryByDirectoryId = useEventCallback((id: DirectoryId): AnyLocalCategory | null => {
    return (
      categories.find((category) => {
        if ('homeDirectoryId' in category) {
          return category.homeDirectoryId === id
        }

        return false
      }) ?? null
    )
  })

  const getCategoriesByType = useEventCallback(
    <T extends AnyLocalCategory['type']>(type: T) =>
      // This is safe, because we know that the result will have the correct type.
      // eslint-disable-next-line no-restricted-syntax
      categories.filter((category) => category.type === type) as CategoryByType<T>[],
  )

  if (localBackend == null) {
    return {
      // We don't have any categories if localBackend is not available.
      categories,
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

  return {
    categories,
    localCategory,
    directories: localCategories,
    addDirectory,
    removeDirectory,
    getCategoryById,
    getCategoriesByType,
    isLocalCategory,
    getCategoryByDirectoryId,
  } as const
}

/**
 * Result of the useCategories hook.
 */
export type CategoriesResult = ReturnType<typeof useCategories>

/**
 * List of all categories.
 */
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

/** Get the API to interact with the categories. */
export function useCategoriesAPI() {
  const context = useContext(CategoriesContext)
  invariant(context != null, 'useCategory must be used within a CategoriesProvider')
  return context
}
