import { proxyRefs } from '$/utils/reactivity'
import { useZustandStoreRef } from '$/utils/zustand'
import { createGlobalState } from '@vueuse/core'
import type { Path } from 'enso-common/src/services/Backend'
import { computed, inject } from 'vue'
import { createStore } from 'zustand'
import { persist } from 'zustand/middleware'

interface LocalRootDirectoryStoreState {
  readonly localRootDirectory: Path | null
  readonly downloadDirectory: Path | null
}

const localRootDirectoryStore = createStore<LocalRootDirectoryStoreState>()(
  persist(
    (): LocalRootDirectoryStoreState => ({
      localRootDirectory: null,
      downloadDirectory: null,
    }),
    { name: 'enso-local-directory', version: 1 },
  ),
)

export type LocalDirectoriesStore = ReturnType<typeof createLocalDirectoriesStore>

function createLocalDirectoriesStore() {
  const defaultDownloadPath = inject<Path>('defaultDownloadPath')

  const localRootDirectory = useZustandStoreRef(
    localRootDirectoryStore,
    (state) => state.localRootDirectory,
  )
  const storedDownloadDirectory = useZustandStoreRef(
    localRootDirectoryStore,
    (state) => state.downloadDirectory,
  )

  const downloadDirectory = computed(() => storedDownloadDirectory.value ?? defaultDownloadPath)

  /** Update the saved local root directory. */
  function setLocalRootDirectory(localRootDirectory: Path | null) {
    localRootDirectoryStore.setState({ localRootDirectory })
  }

  /** Update the saved local root directory. */
  function setDownloadDirectory(downloadDirectory: Path | null) {
    localRootDirectoryStore.setState({ downloadDirectory })
  }

  return proxyRefs({
    localRootDirectory,
    downloadDirectory,
    setLocalRootDirectory,
    setDownloadDirectory,
  })
}

export const useLocalDirectories = createGlobalState(createLocalDirectoriesStore)
