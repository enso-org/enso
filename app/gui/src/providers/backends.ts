import { BackendType } from '#/services/Backend'
import LocalBackend from '#/services/LocalBackend'
import {
  Path,
  PROJECT_MANAGER_LOADING_FAILED_EVENT,
  ProjectManager,
} from '#/services/ProjectManager'
import RemoteBackend from '#/services/RemoteBackend'
import { useEvent } from '@/composables/events'
import { injectGuiConfig, type GuiConfig } from '@/providers/guiConfig'
import { proxyRefs, type ToValue } from '@/util/reactivity'
import { createGlobalState } from '@vueuse/core'
import { HttpClient } from 'enso-common/src/services/HttpClient'
import invariant from 'tiny-invariant'
import { computed, inject, readonly, ref, toValue, watch, watchEffect } from 'vue'
import { useHttpClient } from './httpClient'
import { useText, type GetText } from './text'

export type BackendsStore = ReturnType<typeof useBackends>
function initializeBackends(
  httpClient: HttpClient,
  config: ToValue<GuiConfig>,
  rootDirPath: ToValue<string | undefined>,
  getText: GetText,
) {
  const createProjectManager = (rootPath: string | undefined, projectManagerUrl: string | null) => {
    if (!rootPath) return
    if (projectManagerUrl == null) return
    const rootDirectory = Path(rootPath)
    return new ProjectManager(projectManagerUrl, rootDirectory)
  }
  const projectManager = ref<ProjectManager>()
  watchEffect((onCleanup) => {
    const pm = createProjectManager(toValue(rootDirPath), toValue(config).projectManagerUrl)
    onCleanup(() => pm?.dispose())
    projectManager.value = pm
  })
  const localBackend = computed(() =>
    projectManager.value ? new LocalBackend(getText, projectManager.value) : null,
  )
  const remoteBackend = new RemoteBackend(getText, httpClient)

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

  const didLoadingProjectManagerFail = ref(false)
  useEvent(document, PROJECT_MANAGER_LOADING_FAILED_EVENT, () => {
    didLoadingProjectManagerFail.value = true
  })

  const reconnectToProjectManager = () => {
    // To avoid race conditions, when someone try to reconnect twice in a row.
    invariant(didLoadingProjectManagerFail.value)
    didLoadingProjectManagerFail.value = false
    localBackend.value?.reconnectProjectManager()
  }
  return proxyRefs({
    localBackend,
    remoteBackend,
    backendForType,
    didLoadingProjectManagerFail: readonly(didLoadingProjectManagerFail),
    reconnectToProjectManager,
  })
}

export const useBackends = createGlobalState(() =>
  initializeBackends(useHttpClient(), injectGuiConfig(), inject('rootDirPath'), useText().getText),
)
