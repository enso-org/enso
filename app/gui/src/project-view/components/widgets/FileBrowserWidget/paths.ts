import { findDifferenceIndex } from '@/util/data/array'
import { Opt } from '@/util/data/opt'
import { Err, Ok, Result, unwrapOr, unwrapOrWithLog, withContext } from '@/util/data/result'
import { arrayEquals } from '@/util/equals'
import { ToValue } from '@/util/reactivity'
import type {
  AnyAsset,
  AssetType,
  DirectoryId,
  OrganizationInfo,
  User,
} from 'enso-common/src/services/Backend'
import { assetIsDirectory } from 'enso-common/src/services/Backend'
import { computed, ref, toValue } from 'vue'

function pathToSegments(path: string) {
  const withProtocol = path.split('/')
  if (withProtocol[0] !== 'enso:') return Err(`"${path}" is not an enso path`)
  return Ok(withProtocol.slice(1).filter((segment) => segment))
}

class CannotEnterDir {
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

/**
 * A part of FileBrowserWidget: extracted logic for managing directory stack and current
 * highlight/selection.
 */
export function useFileBrowserStack(
  backend: ToValue<
    Opt<{
      rootPath(user: User): string
      rootDirectoryId(
        user: User,
        organization: OrganizationInfo | null,
        localRootDirectory: null,
      ): DirectoryId | null
    }>
  >,
  choosenPath: ToValue<string>,
  currentUser: ToValue<Opt<User>>,
  writeMode: ToValue<boolean>,
  listDirectory: (dir: Directory) => Promise<readonly AnyAsset<AssetType>[]>,
) {
  const filenameInputContents = ref<string>('')
  const directoryStack = ref<Directory[]>([])
  const isDirectoryStackInitializing = computed(() => directoryStack.value.length === 0)
  const currentDirectory = computed(() => directoryStack.value[directoryStack.value.length - 1])
  const choosenPathSegments = computed(() => pathToSegments(toValue(choosenPath)))

  const rootSegments = computed(() => {
    const user = toValue(currentUser)
    if (!user) return
    const root = toValue(backend)?.rootPath(user) ?? 'enso://'
    return pathToSegments(root)
  })
  const currentDirSegments = computed(() => {
    if (!rootSegments.value?.ok) return
    return [...rootSegments.value.value, ...directoryStack.value.slice(1).map((dir) => dir.title)]
  })

  const currentPath = computed(() => {
    if (currentDirSegments.value == null) return
    return `enso://${currentDirSegments.value.map((dir) => `${dir}/`).join('')}`
  })

  const highlightedName = computed(() => {
    if (toValue(writeMode)) return filenameInputContents.value
    else {
      if (
        currentDirSegments.value != null &&
        choosenPathSegments.value?.ok &&
        choosenPathSegments.value.value.length === currentDirSegments.value.length + 1 &&
        arrayEquals(currentDirSegments.value, choosenPathSegments.value.value.slice(0, -1))
      ) {
        return choosenPathSegments.value.value[choosenPathSegments.value.value.length - 1]
      } else {
        return undefined
      }
    }
  })

  const currentFilePath = computed(
    () =>
      filenameInputContents.value &&
      currentPath.value &&
      `${currentPath.value}${filenameInputContents.value}`,
  )

  async function enterDirByName(
    name: string,
    stack: Directory[],
  ): Promise<Result<void, CannotEnterDir>> {
    const currentDir = stack[stack.length - 1]
    if (currentDir == null) return Err(new CannotEnterDir('emptyStack', name))
    const content = await listDirectory(currentDir)
    const nextAsset = content.find((asset) => asset.title === name)
    if (!nextAsset) return Err(new CannotEnterDir('notFound', name))
    if (!assetIsDirectory(nextAsset)) return Err(new CannotEnterDir('notDir', name))
    stack.push(nextAsset)
    return Ok()
  }

  function dirsToEnterOnInit(user: User) {
    const initialSegments = unwrapOr(choosenPathSegments.value, ['Users', user.name])
    const rootSegs = unwrapOrWithLog(rootSegments.value ?? Err('cannot load root directory'), [])
    const afterRootIndex = findDifferenceIndex(initialSegments, rootSegs)
    if (afterRootIndex < rootSegs.length) {
      return []
    } else {
      return initialSegments.slice(afterRootIndex)
    }
  }

  async function initializeStack(
    user: User | null,
    organization: OrganizationInfo | null,
  ): Promise<Result> {
    return withContext(
      () => 'Cannot enter initial directory',
      async (): Promise<Result<undefined, unknown>> => {
        if (!user) {
          return Err('Cannot load file list: not logged in.')
        }
        const rootDirectoryId =
          toValue(backend)?.rootDirectoryId(user, organization, null) ?? user.rootDirectoryId

        const stack = [{ id: rootDirectoryId, title: 'Cloud' }]
        const toEnter = dirsToEnterOnInit(user)
        for (const [index, name] of toEnter.entries()) {
          const result = await enterDirByName(name, stack)
          if (result.ok) continue
          const breakReason = result.error.payload.reason
          if (
            breakReason === 'notDir' ||
            (breakReason === 'notFound' && index == toEnter.length - 1)
          ) {
            filenameInputContents.value = name
          } else if (breakReason != 'notFound') {
            return result
          }
          break
        }
        directoryStack.value = stack
        return Ok()
      },
    )
  }

  return {
    filenameInputContents,
    directoryStack,
    currentDirectory,
    currentPath,
    currentFilePath,
    highlightedName,
    initializeStack,
    isDirectoryStackInitializing,
  }
}
