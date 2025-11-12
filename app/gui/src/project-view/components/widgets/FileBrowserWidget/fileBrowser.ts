/** @file Low-level logic for the File Browser Widget. */
import {
  type EnsoPath,
  ensoPathEqual,
  mapPath,
} from '@/components/widgets/FileBrowserWidget/ensoPath'
import type { Directory } from '@/components/widgets/FileBrowserWidget/pathBrowsing'
import type { Mutation } from '@/composables/backend'
import type { ToValue } from '@/util/reactivity'
import { useToast } from '@/util/toast'
import { Backend, type CreatedDirectory } from 'enso-common/src/services/Backend'
import type { Result } from 'enso-common/src/utilities/data/result'
import { computed, ref, toValue } from 'vue'

/** @returns Reactive query parameters to list the given directory's contents. */
export function listDirectoryArgs(params: ToValue<Directory | undefined>) {
  return computed<Parameters<Backend['listDirectory']> | undefined>(() => {
    const paramsValue = toValue(params)
    return paramsValue ?
        [
          {
            parentId: paramsValue.id,
            filterBy: null,
            labels: null,
            recentProjects: false,
            from: null,
            pageSize: null,
            sortDirection: null,
            sortExpression: null,
          },
          paramsValue.title,
        ]
      : undefined
  })
}

/** Manages the current path of the file browser. */
export function useCurrentPath({
  home,
  enteredPath,
}: {
  home: ToValue<Result<EnsoPath>>
  enteredPath: ToValue<EnsoPath | undefined>
}) {
  const fallbackPath = computed(() => {
    const defaultPath = toValue(home)
    if (!defaultPath.ok) return undefined
    return {
      dirs: defaultPath.value,
      file: '',
    }
  })

  function effectivePath(inputPath: Result<EnsoPath>) {
    if (!inputPath.ok) return fallbackPath.value
    const file = inputPath.value.segments.pop() ?? ''
    const dirs = inputPath.value
    return { dirs, file }
  }

  const chosenFile = ref<{ path: EnsoPath; name: string }>()
  const currentDirPath = ref<EnsoPath>()

  function setPath(inputPath: Result<EnsoPath>) {
    const path = effectivePath(inputPath)
    currentDirPath.value = path?.dirs
    chosenFile.value =
      path?.file ?
        {
          path: path.dirs,
          name: path.file,
        }
      : undefined
  }

  function updateChosenPath(f: (segments: string[]) => string[]) {
    const base = toValue(enteredPath)
    currentDirPath.value = base && mapPath(base, f)
  }

  const append =
    <T>(...values: T[]) =>
    <T>(segments: T[]) => [...segments, ...values]
  function enterDir({ title }: { title: string }) {
    updateChosenPath(append(title))
  }

  const sliceTo =
    (length: number) =>
    <T>(segments: T[]) =>
      segments.slice(0, length)
  function popTo(length: number) {
    updateChosenPath(sliceTo(length))
  }

  const chosenFilename = computed(
    () =>
      (chosenFile.value != null &&
        ensoPathEqual(currentDirPath.value, chosenFile.value.path) &&
        chosenFile.value.name) ||
      null,
  )

  return { currentDirPath, chosenFilename, setPath, enterDir, popTo, append }
}

/** Supports creating a new secret with the specified content in the current directory. */
export function useSecretCreation({
  currentDirectory,
  mutation,
}: {
  currentDirectory: ToValue<Directory | undefined>
  mutation: Mutation
}) {
  const errorToast = useToast.error()
  const commitSecretPending = ref(false)
  const createSecret = mutation('createSecret', { meta: { awaitInvalidates: false } })
  async function commitSecret(value: string, name: string, onSuccess: () => void) {
    commitSecretPending.value = true
    try {
      await createSecret.mutateAsync([
        { name, value, parentDirectoryId: toValue(currentDirectory)?.id ?? null },
      ])
    } catch (error) {
      errorToast.show(`Failed to create secret: ${error instanceof Error ? error.message : error}`)
      return
    } finally {
      commitSecretPending.value = false
    }
    onSuccess()
  }
  return { commitSecret, commitSecretPending }
}

/** Supports creating or renaming a directory. */
export function useUpsertDirectory({
  currentDirectory,
  mutation,
}: {
  currentDirectory: ToValue<Directory | undefined>
  mutation: Mutation
}) {
  // Don't await invalidates, because we want `createDirectory` to return first, to fill
  // `keyOverride` property before getting update from backend.
  const createDir = mutation('createDirectory', { meta: { awaitInvalidates: false } })
  const updateDir = mutation('updateDirectory')

  function acceptName(
    editedAsset: Directory | undefined,
    name: string,
    handler: (action: Promise<CreatedDirectory | null>) => void,
  ) {
    const parentId = toValue(currentDirectory)?.id
    if (parentId == null) {
      console.error('Cannot rename directory without parentId')
      return
    }
    const action =
      editedAsset == null ? createDir.mutateAsync([{ title: name, parentId }, false])
      : editedAsset.title != name ?
        updateDir.mutateAsync([editedAsset.id, { title: name }, editedAsset.title])
      : Promise.resolve(null)
    handler(action)
  }

  return { acceptName }
}
