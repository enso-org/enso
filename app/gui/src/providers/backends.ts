import { BackendType } from '#/services/Backend'
import LocalBackend from '#/services/LocalBackend'
import ProjectManager, {
  ProjectManagerEvents,
  Path as ProjectManagerPath,
} from '#/services/ProjectManager'
import RemoteBackend from '#/services/RemoteBackend'
import HttpClient from '#/utilities/HttpClient'
import { useEvent } from '@/composables/events'
import { createContextStore } from '@/providers'
import { GuiConfig } from '@/providers/guiConfig'
import { ToValue } from '@/util/reactivity'
import invariant from 'tiny-invariant'
import { computed, proxyRefs, readonly, ref, toValue, watchEffect } from 'vue'
import { GetText } from './text'

export type BackendsStore = ReturnType<typeof useBackends>
function useBackends(
  httpClient: HttpClient,
  config: ToValue<GuiConfig>,
  rootDirPath: ToValue<string | undefined>,
  getText: GetText,
) {
  const createProjectManager = (rootPath: string | undefined, projectManagerUrl: string | null) => {
    if (!rootPath) return
    if (projectManagerUrl == null) return
    const rootDirectory = ProjectManagerPath(rootPath)
    return new ProjectManager(projectManagerUrl, rootDirectory)
  }
  const projectManager = ref<ProjectManager>()
  watchEffect((onCleanup) => {
    const pm = createProjectManager(toValue(rootDirPath), toValue(config).projectManagerUrl)
    onCleanup(() => pm?.dispose())
    projectManager.value = pm
  })
  const localBackend = computed(() =>
    projectManager.value ? new LocalBackend(projectManager.value) : null,
  )
  const remoteBackend = new RemoteBackend(httpClient, console, getText)

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
  useEvent(document, ProjectManagerEvents.loadingFailed, () => {
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

export const [provideBackends, injectBackends] = createContextStore('backends', useBackends)
