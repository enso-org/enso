import { proxyRefs } from '$/utils/reactivity'
import { useZustandStoreRef } from '$/utils/zustand'
import { createGlobalState } from '@vueuse/core'
import type { Path } from 'enso-common/src/services/Backend'
import { computed, inject } from 'vue'
import { createStore } from 'zustand'
import { persist } from 'zustand/middleware'

interface LocalPathsStoreState {
  readonly localRootDirectory: Path | null
  readonly downloadDirectory: Path | null
}

const localPathsStore = createStore<LocalPathsStoreState>()(
  persist(
    (): LocalPathsStoreState => ({
      localRootDirectory: null,
      downloadDirectory: null,
    }),
    { name: 'enso-local-directory', version: 1 },
  ),
)

export type LocalPathsStore = ReturnType<typeof createLocalPathsStore>

function createLocalPathsStore() {
  const defaultDownloadPath = inject<Path>('defaultDownloadPath')

  const localRootDirectory = useZustandStoreRef(
    localPathsStore,
    (state) => state.localRootDirectory,
  )
  const storedDownloadDirectory = useZustandStoreRef(
    localPathsStore,
    (state) => state.downloadDirectory,
  )

  const downloadDirectory = computed(() => storedDownloadDirectory.value ?? defaultDownloadPath)

  /** Update the saved local root directory. */
  function setLocalRootDirectory(localRootDirectory: Path | null) {
    localPathsStore.setState({ localRootDirectory })
  }

  /** Update the saved local root directory. */
  function setDownloadDirectory(downloadDirectory: Path | null) {
    localPathsStore.setState({ downloadDirectory })
  }

  return proxyRefs({
    localRootDirectory,
    downloadDirectory,
    setLocalRootDirectory,
    setDownloadDirectory,
  })
}

export const useLocalPaths = createGlobalState(createLocalPathsStore)
