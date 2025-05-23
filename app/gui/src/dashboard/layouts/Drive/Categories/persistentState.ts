/** @file Persistent state for the Drive. */
import { useStore } from '#/hooks/storeHooks'
import type { Path } from '#/services/Backend'
import { createStore } from 'zustand'
import { persist } from 'zustand/middleware'

/** State for {@link categoryIdStore}. */
interface LocalRootDirectoryStoreState {
  readonly localDirectories: readonly Path[]
}

const localRootDirectoryStore = createStore<LocalRootDirectoryStoreState>()(
  persist(
    (): LocalRootDirectoryStoreState => ({
      localDirectories: [],
    }),
    { name: 'enso-local-root-directory', version: 1 },
  ),
)

/** The saved local directories. */
export function useLocalDirectories() {
  return useStore(localRootDirectoryStore, ({ localDirectories }) => localDirectories)
}

/** Update the saved local directories. */
export function setLocalDirectories(localDirectories: readonly Path[]) {
  localRootDirectoryStore.setState({ localDirectories })
}
