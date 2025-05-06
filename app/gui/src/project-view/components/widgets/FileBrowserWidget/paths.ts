import { type Opt } from '@/util/data/opt'
import { Err, Ok, Result } from '@/util/data/result'
import { type ToValue } from '@/util/reactivity'
import type {
  AnyAsset,
  DirectoryAsset,
  DirectoryId,
  OrganizationInfo,
  User,
} from 'enso-common/src/services/Backend'
import { assetIsDirectory } from 'enso-common/src/services/Backend'
import { computed, reactive, ref, toRaw, toValue, type ComputedRef, type Ref } from 'vue'

export function pathToSegments(path: string) {
  const withProtocol = path.split('/')
  if (withProtocol[0] !== 'enso:') return Err(`"${path}" is not an enso path`)
  return Ok(withProtocol.slice(1).filter((segment) => segment))
}

export class CannotEnterDir {
  constructor(
    public reason: 'emptyStack' | 'notFound' | 'notDir',
    public name: string,
  ) {}

  toString() {
    switch (this.reason) {
      case 'emptyStack':
        return 'Stack is empty'
      case 'notFound':
        return `Directory "${this.name}" not found`
      case 'notDir':
        return `"${this.name}" is not a directory`
    }
  }
}

/** A directory on browser's stack. */
export interface Directory {
  id: DirectoryId
  title: string
}

interface UserFilesBackend {
  rootPath: (user: User) => string
  rootDirectoryId: (
    user: User,
    organization: OrganizationInfo | null,
    localRootDirectory: null,
  ) => DirectoryId | null
}

interface QueryResult<T> {
  data: ToValue<T>
  isFetched: ToValue<boolean>
  error: ToValue<Error | null>
}

export type PathSegment = string

export interface UserFiles {
  rootPath: ToValue<string>
  home: ToValue<PathSegment[]>
  rootDirectoryId: ToValue<DirectoryId>
}

/** @returns An API for getting information about the logged-in user's files. */
export function useUserFiles({
  backend,
  user,
  organization,
}: {
  backend: ToValue<UserFilesBackend | null>
  user: QueryResult<Opt<User>>
  organization: QueryResult<Opt<OrganizationInfo>>
}): { userFiles: ComputedRef<UserFiles | null>; userFilesError: ComputedRef<Error | null> } {
  function userFiles(backend: UserFilesBackend, user: User): UserFiles {
    return {
      rootPath: computed<string>(() => backend.rootPath(user)),
      rootDirectoryId: computed<DirectoryId>(() => {
        const currentOrganization = toValue(organization.data)
        return (
          (currentOrganization && backend.rootDirectoryId(user, currentOrganization, null)) ??
          user.rootDirectoryId
        )
      }),
      /** The user's home directory. */
      home: computed<PathSegment[]>(() => ['Users', user.name]),
    }
  }

  return {
    userFiles: computed<UserFiles | null>(() => {
      if (!toValue(user.isFetched) || !toValue(organization.isFetched)) return null
      const currentBackend = toValue(backend)
      if (!currentBackend) return null
      const currentUser = toValue(user.data)
      if (!currentUser) return null
      return userFiles(currentBackend, currentUser)
    }),
    userFilesError: computed<Error | null>(
      () => toValue(user.error) ?? toValue(organization.error),
    ),
  }
}

export interface PathBrowsing {
  setBrowsingPath: (path: PathSegment[], root: Directory) => Promise<Result<void, CannotEnterDir>>
  /** The entered directories, from after the root through the current directory. */
  enteredPath: Readonly<Ref<PathSegment[]>>
  /**
   * Any unentered trailing portion of the path; this starts with any referenced directories that
   * were not found to exist, and ends with any non-directory element present.
   */
  unenteredPath: Readonly<Ref<string>>
  currentDirectory: Readonly<Ref<Directory | undefined>>
  isPending: Readonly<Ref<boolean>>
}

export function usePathBrowsing({
  listDirectory,
}: {
  listDirectory: (dir: Directory) => Promise<readonly AnyAsset[]>
}): PathBrowsing {
  const enteredDirectories = reactive<Directory[]>([])
  const unenteredPath = ref('')
  const isPending = ref(true)

  async function getChildDirectory(
    name: string,
    parent: Directory,
  ): Promise<Result<DirectoryAsset, CannotEnterDir>> {
    const content = await listDirectory(parent)
    const nextAsset = content.find((asset) => asset.title === name)
    if (!nextAsset) return Err(new CannotEnterDir('notFound', name))
    if (!assetIsDirectory(nextAsset)) return Err(new CannotEnterDir('notDir', name))
    return Ok(nextAsset)
  }

  async function setBrowsingPath(
    path: PathSegment[],
    root: Directory,
  ): Promise<Result<void, CannotEnterDir>> {
    const oldDirectories = toRaw(enteredDirectories)
    if (root.id !== oldDirectories[0]?.id) enteredDirectories.length = 0
    enteredDirectories[0] = root
    let i = 1
    isPending.value = true
    let result: Result<void, CannotEnterDir> = Ok()
    for (const title of path) {
      if (oldDirectories[i]?.title !== title) {
        enteredDirectories.length = i
        const thisResult = await getChildDirectory(title, oldDirectories[i - 1]!)
        if (!thisResult.ok) {
          result = thisResult
          const breakReason = thisResult.error.payload.reason
          if (breakReason === 'notDir' || (breakReason === 'notFound' && i === path.length - 1))
            unenteredPath.value = title
          break
        }
        enteredDirectories.push({
          id: thisResult.value.id,
          title,
        })
      }
      i += 1
    }
    enteredDirectories.length = i
    isPending.value = false
    return result
  }

  return {
    setBrowsingPath,
    enteredPath: computed(() => enteredDirectories.map(({ title }) => title)),
    unenteredPath,
    currentDirectory: computed(() => enteredDirectories[enteredDirectories.length - 1]),
    isPending,
  }
}
