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
    /** @deprecated Prefer `useLocalDirectories` and `setLocalDirectories`. */
    readonly localRootDirectories: z.infer<typeof LOCAL_ROOT_DIRECTORIES_SCHEMA>
  }
}

const LOCAL_ROOT_DIRECTORIES_SCHEMA = z.string().array().readonly()

LocalStorage.registerKey('localRootDirectories', { schema: LOCAL_ROOT_DIRECTORIES_SCHEMA })

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
    { name: 'enso-local-saved-directories', version: 1 },
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
