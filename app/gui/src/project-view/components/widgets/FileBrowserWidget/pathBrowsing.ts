/** @file APIs for navigating a cloud directory structure. */
import type { EnsoPath } from '@/components/widgets/FileBrowserWidget/ensoPath'
import { findDifferenceIndex } from '@/util/data/array'
import {
  assetIsDirectory,
  type DirectoryAsset,
  type DirectoryId,
  type ListDirectoryResponseBody,
} from 'enso-common/src/services/Backend'
import { Err, Ok, type Result } from 'enso-common/src/utilities/data/result'
import { computed, reactive, ref, toRaw, type Ref } from 'vue'

/** A directory on browser's stack. */
export interface Directory {
  id: DirectoryId
  title: string
}

class CannotEnterDir {
  constructor(
    public reason: 'notFound' | 'notDir',
    public name: string,
  ) {}

  toString() {
    switch (this.reason) {
      case 'notFound':
        return `Directory "${this.name}" not found`
      case 'notDir':
        return `"${this.name}" is not a directory`
    }
  }
}

export interface PathBrowsing {
  setBrowsingPath: (path: EnsoPath) => Promise<Result<void, CannotEnterDir>>
  /** The entered path; this will be the path provided to `setBrowsingPath`, or some prefix. */
  enteredPath: Readonly<Ref<EnsoPath | undefined>>
  /**
   * Any unentered trailing portion of the path; this starts with any referenced directories that
   * were not found to exist, and ends with any non-directory element present.
   */
  unenteredPathSuffix: Readonly<Ref<string>>
  /** The deepest {@link Directory} of the {@link enteredPath} (which may be the root). */
  currentDirectory: Readonly<Ref<Directory | undefined>>
  /**
   * `true` until entering the path most recently set by `setBrowsingPath` has been completed.
   *
   * While this value is `true`, the other outputs may reflect only part of the input; however, they
   * should not contain stale information unrelated to the current path.
   */
  isPending: Readonly<Ref<boolean>>
}

/** @returns An API that supports entering a path. */
export function usePathBrowsing({
  listDirectory,
}: {
  listDirectory: (dir: Directory) => Promise<ListDirectoryResponseBody | null>
}): PathBrowsing {
  const enteredDirectories = reactive<Directory[]>([])
  const unenteredPathSuffix = ref('')
  const isPending = ref(true)
  const root = ref<DirectoryId>()

  async function getChildDirectory(
    name: string,
    parent: Directory,
  ): Promise<Result<DirectoryAsset, CannotEnterDir>> {
    const content = (await listDirectory(parent))?.assets ?? []
    const nextAsset = content.find((asset) => asset.title === name)
    if (!nextAsset) return Err(new CannotEnterDir('notFound', name))
    if (!assetIsDirectory(nextAsset)) return Err(new CannotEnterDir('notDir', name))
    return Ok(nextAsset)
  }

  async function setBrowsingPath(path: EnsoPath): Promise<Result<void, CannotEnterDir>> {
    const oldDirectories = toRaw(enteredDirectories)
    if (path.root !== root.value) {
      enteredDirectories.length = 0
      root.value = path.root
    }
    isPending.value = true
    unenteredPathSuffix.value = ''
    const firstDifferent = findDifferenceIndex(
      oldDirectories.map(({ title }) => title),
      path.segments,
    )
    enteredDirectories.length = firstDifferent
    let i = firstDifferent
    let prevDir = oldDirectories[i - 1] ?? { title: '<directory>', id: path.root }
    try {
      for (const title of path.segments.slice(i)) {
        const result = await getChildDirectory(title, prevDir)
        if (!result.ok) {
          const breakReason = result.error.payload.reason
          if (breakReason === 'notDir' || breakReason === 'notFound') {
            unenteredPathSuffix.value = path.segments.slice(i).join('/')
            break
          } else {
            return result
          }
        }
        const dir = {
          id: result.value.id,
          title,
        }
        enteredDirectories.push(dir)
        prevDir = dir
        i += 1
      }
    } finally {
      isPending.value = false
    }
    return Ok()
  }

  return {
    setBrowsingPath,
    enteredPath: computed(
      () =>
        root.value && {
          root: root.value,
          segments: enteredDirectories.map(({ title }) => title),
        },
    ),
    unenteredPathSuffix,
    currentDirectory: computed(
      () =>
        enteredDirectories[enteredDirectories.length - 1] ??
        (root.value != null ? { title: 'Cloud', id: root.value } : undefined),
    ),
    isPending,
  }
}
