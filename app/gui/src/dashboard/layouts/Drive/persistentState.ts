/** @file Persistent state for the Drive. */
import { useStore } from '#/hooks/storeHooks'
import LocalStorage from '#/utilities/LocalStorage'
import { Path } from 'enso-common/src/services/Backend'
import { z } from 'zod'
import { createStore } from 'zustand'
import { persist } from 'zustand/middleware'

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    /** @deprecated Prefer `useLocalRootDirectory` and `setLocalRootDirectory`. */
    readonly localRootDirectory: string
  }
}
LocalStorage.registerKey('localRootDirectory', { schema: z.string() })

/** State for {@link categoryIdStore}. */
interface LocalRootDirectoryStoreState {
  readonly localRootDirectory: Path | null
  readonly downloadDirectory: Path | null
}

export const localRootDirectoryStore = createStore<LocalRootDirectoryStoreState>()(
  persist(
    (): LocalRootDirectoryStoreState => ({
      localRootDirectory: (() => {
        const oldPath = LocalStorage.getInstance().get('localRootDirectory')
        return oldPath != null ? Path(oldPath) : null
      })(),
      downloadDirectory: null,
    }),
    { name: 'enso-local-directory', version: 1 },
  ),
)

/** The saved local root directory. */
export function useLocalRootDirectory() {
  return useStore(localRootDirectoryStore, ({ localRootDirectory }) => localRootDirectory)
}

/** Update the saved local root directory. */
export function setLocalRootDirectory(localRootDirectory: Path | null) {
  localRootDirectoryStore.setState({ localRootDirectory })
}

/** Update the saved local root directory. */
export function setDownloadDirectory(downloadDirectory: Path | null) {
  localRootDirectoryStore.setState({ downloadDirectory })
}
