/** @file Persistent state for the Drive. */
import { useStore } from '#/hooks/storeHooks'
import type { Path } from '#/services/Backend'
import { createStore } from 'zustand'
import { persist } from 'zustand/middleware'

/** State for {@link categoryIdStore}. */
interface LocalRootDirectoryStoreState {
  readonly localRootDirectory: Path | null
}

const localRootDirectoryStore = createStore<LocalRootDirectoryStoreState>()(
  persist((): LocalRootDirectoryStoreState => ({ localRootDirectory: null }), {
    name: 'enso-local-root-directory',
    version: 1,
  }),
)

/** The saved local root directory. */
export function useLocalRootDirectory() {
  return useStore(localRootDirectoryStore, ({ localRootDirectory }) => localRootDirectory)
}

/** Update the saved local root directory. */
export function setLocalRootDirectory(localRootDirectory: Path | null) {
  localRootDirectoryStore.setState({ localRootDirectory })
}
