import { localRootDirectoryStore } from '#/layouts/Drive/persistentState'
import { download } from '#/utilities/download'
import { proxyRefs, type ToValue } from '@/util/reactivity'
import { createGlobalState } from '@vueuse/core'
import { BackendType, DirectoryId, Path } from 'enso-common/src/services/Backend'
import { HttpClient } from 'enso-common/src/services/HttpClient'
import { LocalBackend } from 'enso-common/src/services/LocalBackend'
import { ProjectManager } from 'enso-common/src/services/ProjectManager/ProjectManager'
import { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { extractIdFromDirectoryId } from 'enso-common/src/services/RemoteBackend/ids'
import invariant from 'tiny-invariant'
import { computed, inject, ref, toValue, watch, watchEffect } from 'vue'
import { useHttpClient } from './httpClient'
import { useText, type GetText } from './text'

export type BackendsStore = ReturnType<typeof useBackends>
function initializeBackends(
  httpClient: HttpClient,
  rootDirPath: ToValue<string | undefined>,
  getText: GetText,
) {
  const createProjectManager = (rootPath: string | undefined) => {
    if (!rootPath) return
    const rootDirectory = Path(rootPath)
    return new ProjectManager(rootDirectory)
  }
  const projectManager = ref<ProjectManager>()
  watchEffect(() => {
    const pm = createProjectManager(toValue(rootDirPath))
    projectManager.value = pm
  })
  const localBackend = computed(() =>
    projectManager.value ?
      new LocalBackend(
        getText,
        projectManager.value,
        undefined,
        download,
        () => localRootDirectoryStore.getState().localRootDirectory,
        window.api?.system?.getFilePath,
      )
    : null,
  )
  const remoteBackend = new RemoteBackend({
    apiUrl: $config.API_URL ?? '',
    getText,
    client: httpClient,
    downloader: download,
    downloadCloudProject: async function downloadCloudProject(this: RemoteBackend, params) {
      const queryString = new URLSearchParams(params)
      const response = await this.get<{
        readonly projectRootDirectory: string
        readonly parentDirectory: string
      }>(new URL(`/api/cloud/download-project?${queryString}`, location.href).toString())
      if (!response.ok) {
        return await this.throw(response, 'resolveProjectAssetPathBackendError')
      }
      return await response.json()
    },
    getProjectArchive: async function getProjectArchive(
      this: RemoteBackend,
      directoryId: DirectoryId,
      fileName: string,
    ): Promise<File> {
      const queryString = new URLSearchParams({
        directory: extractIdFromDirectoryId(directoryId),
      }).toString()
      const response = await this.get(
        new URL(`/api/cloud/get-project-archive?${queryString}`, location.href).toString(),
      )
      if (!response.ok) {
        return await this.throw(response, 'resolveProjectAssetPathBackendError')
      }
      const responseBody = await response.arrayBuffer()
      return new File([responseBody], fileName)
    },
  })

  watch(
    () => getText,
    (getText) => {
      localBackend.value?.setGetText(getText)
      remoteBackend.setGetText(getText)
    },
  )

  const backendForType = (projectType: BackendType) => {
    switch (projectType) {
      case BackendType.remote:
        return remoteBackend
      case BackendType.local: {
        invariant(
          localBackend.value,
          'Attempted to get a local backend for local project, but no local backend was provided.',
        )
        return localBackend.value
      }
    }
  }

  return proxyRefs({
    localBackend,
    remoteBackend,
    backendForType,
  })
}

export const useBackends = createGlobalState(() =>
  initializeBackends(useHttpClient(), inject('rootDirPath'), useText().getText),
)
