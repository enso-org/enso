/** @file Persistent state for the Drive. */
import { useStore } from '#/hooks/storeHooks'
import { Path } from '#/services/Backend'
import LocalStorage from '#/utilities/LocalStorage'
import { createStore } from 'zustand'
import { persist } from 'zustand/middleware'

/** State for {@link categoryIdStore}. */
interface LocalRootDirectoryStoreState {
  readonly localDirectories: readonly Path[]
}

const localRootDirectoryStore = createStore<LocalRootDirectoryStoreState>()(
  persist(
    (): LocalRootDirectoryStoreState => ({
      localDirectories:
        LocalStorage.getInstance()
          .get('localRootDirectories')
          ?.map((directory) => Path(directory)) ?? [],
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
