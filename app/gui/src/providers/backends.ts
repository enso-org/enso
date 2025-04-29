import { isCloudCategory, type Category } from '#/layouts/CategorySwitcher/Category'
import { BackendType } from '#/services/Backend'
import LocalBackend from '#/services/LocalBackend'
import ProjectManager, {
  ProjectManagerEvents,
  Path as ProjectManagerPath,
} from '#/services/ProjectManager'
import RemoteBackend from '#/services/RemoteBackend'
import HttpClient from '#/utilities/HttpClient'
import { createContextStore } from '@/providers'
import { GuiConfig } from '@/providers/guiConfig'
import { ToValue } from '@/util/reactivity'
import * as common from 'enso-common'
import invariant from 'tiny-invariant'
import { computed, markRaw, onScopeDispose, readonly, ref, toValue } from 'vue'

function useBackends(
  httpClient: HttpClient,
  config: ToValue<GuiConfig>,
  rootDirPath: ToValue<string | undefined>,
) {
  const projectManager = computed(() => {
    const rootPath = toValue(rootDirPath)
    if (rootPath == null) return
    const cfg = toValue(config)
    if (cfg.projectManagerUrl == null) return
    const rootDirectory = ProjectManagerPath(rootPath)
    if (rootDirPath && cfg.projectManagerUrl != null)
      return new ProjectManager(cfg.projectManagerUrl, rootDirectory)
    else return null
  })
  const localBackend = computed(() =>
    projectManager.value ? markRaw(new LocalBackend(projectManager.value)) : null,
  )
  // TODO getText
  const remoteBackend = markRaw(new RemoteBackend(httpClient, console, () => ''))

  const backendByCategory = (category: Category) =>
    pickBackend(category, remoteBackend, localBackend.value)

  const backendForProjectType = (projectType: BackendType) => {
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
  const onProjectManagerLoadingFailed = () => {
    didLoadingProjectManagerFail.value = true
  }
  document.addEventListener(ProjectManagerEvents.loadingFailed, onProjectManagerLoadingFailed)
  onScopeDispose(() =>
    document.removeEventListener(ProjectManagerEvents.loadingFailed, onProjectManagerLoadingFailed),
  )

  const reconnectToProjectManager = () => {
    didLoadingProjectManagerFail.value = false
    localBackend.value?.reconnectProjectManager()
  }
  return {
    localBackend,
    remoteBackend,
    backendByCategory,
    backendForProjectType,
    didLoadingProjectManagerFail: readonly(didLoadingProjectManagerFail),
    reconnectToProjectManager,
  }
}

export const [provideBackends, injectBackends] = createContextStore('backends', useBackends)

/**
 * Pick the backend for the given category.
 * @throws {Error} when a Local Backend is requested for a non-local project.
 */
export function pickBackend(
  category: Category,
  remoteBackend: RemoteBackend,
  localBackend: LocalBackend | null,
) {
  if (isCloudCategory(category)) {
    return remoteBackend
  }

  invariant(
    localBackend != null,
    `This distribution of ${common.PRODUCT_NAME} does not support the Local Backend.`,
  )

  return localBackend
}
