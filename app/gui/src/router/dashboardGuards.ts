import { useAuth } from '$/providers/auth'
import { useBackends } from '$/providers/backends'
import { useConfig } from '$/providers/config'
import { routeFromTab, useContainerData, type Tab } from '$/providers/container'
import { backendQueryOptions } from '@/composables/backend'
import { onlineManager, useQueryClient } from '@tanstack/vue-query'
import {
  AssetType,
  Backend,
  EnsoPath,
  extractTypeFromId,
  isProjectId,
  isRemoteAssetPath,
  Path,
  Plan,
  ProjectId,
  type AssetDetailsResponse,
  type User,
} from 'enso-common/src/services/Backend'
import {
  isLocalProjectId,
  newDirectoryId,
  type LocalBackend,
} from 'enso-common/src/services/LocalBackend'
import type { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { platform, Platform } from 'enso-common/src/utilities/detect'
import { getFileName } from 'enso-common/src/utilities/file'
import type { NavigationGuardReturn, RouteLocation } from 'vue-router'

export const SAMPLES_DIRECTORY = 'Samples'
export const LOCAL_WELCOME_PROJECT_RELATIVE_PATH = `${SAMPLES_DIRECTORY}/Getting_Started`
export const CLOUD_WELCOME_PROJECT_RELATIVE_PATH = `${SAMPLES_DIRECTORY}/Getting Started.project`

type BackendAPI<B extends Backend> = Pick<B, 'rootPath' | 'listDirectory'>

/** Redirect from "ensoPath" route to proper asset's tab for this route. */
export async function redirectFromPath(to: RouteLocation) {
  if (to.params.path == null) return false
  const auth = useAuth()
  const { localBackend, remoteBackend } = useBackends()

  const path = EnsoPath(to.params.path instanceof Array ? to.params.path.join('/') : to.params.path)
  if (!path) return false

  const backend = isRemoteAssetPath(path) ? remoteBackend : localBackend
  if (backend == null) return false
  await auth.waitForSession()
  const resolvedPath = await backend.resolveEnsoPath(path).catch(() => null)
  const typedAsset = resolvedPath && extractTypeFromId(resolvedPath.id)
  switch (typedAsset?.type) {
    case AssetType.project:
      return { name: 'project', params: { id: typedAsset.id } }
    default:
      return { name: 'dashboard' }
  }
}

/** Redirect from "default" route to some opened tab (if any). */
export async function maybeRedirectToTab(to: RouteLocation) {
  if (to.name !== 'dashboard') return
  const containerData = useContainerData()
  const tab = containerData.nextTab
  if (tab != null) return routeFromTab(tab, to)
}

/** Get path of the project to auto-open on application launch. */
export async function welcomeProjectPath(
  cliStartupProject: string | undefined,
  user: User,
  {
    localBackend,
    remoteBackend,
  }: {
    localBackend: BackendAPI<LocalBackend> | null
    remoteBackend: BackendAPI<RemoteBackend>
  },
) {
  let path: string | undefined

  if (cliStartupProject?.startsWith('file:')) return undefined
  // If not file url, we expect this parameter to be a project name
  if (cliStartupProject) {
    path = `${localBackend?.rootPath()}/${cliStartupProject}`
  } else {
    if (
      await shouldOpenWelcomeProject(localBackend, user.plan === Plan.free ? null : remoteBackend)
    ) {
      if (user.plan === Plan.free) {
        path = `${localBackend?.rootPath()}/${LOCAL_WELCOME_PROJECT_RELATIVE_PATH}`
      } else {
        path = `enso://Users/${user.name}/${CLOUD_WELCOME_PROJECT_RELATIVE_PATH}`
      }
    }
  }

  return path
}

/**
 * A navigation guard for Dashboard page, which handles possible redirection to initial project
 * if any should be opened.
 *
 * It may be a project specified in CLI arguments or the Welcome project on fresh installs.
 */
export async function maybeRedirectToProject(to: RouteLocation): Promise<NavigationGuardReturn> {
  // Do not look for project if we already redirecting from somewhere to avoid redirect loop.
  if (to.name !== 'dashboard' || to.redirectedFrom != null) return
  const backends = useBackends()
  const config = useConfig()
  const auth = useAuth()
  await auth.waitForSession()

  // In case of not being logged in, the redirection should be managed by ProtectedLayout.
  if (auth.session == null) return

  const pathFromOptions =
    config.params.startup.project && backends.localBackend ?
      await uploadProjectArchive(config.params.startup.project, backends.localBackend)
    : undefined

  const initialPath =
    pathFromOptions ??
    (await welcomeProjectPath(config.params.startup.project, auth.session.user, backends))

  return initialPath ? { name: 'ensoPath', params: { path: initialPath.split('/') } } : true
}

async function shouldOpenWelcomeProject(
  localBackend: Pick<LocalBackend, 'listDirectory'> | null,
  remoteBackend: Pick<RemoteBackend, 'listDirectory'> | null,
) {
  const navigatedInDrive =
    window.localStorage.getItem('enso-category-id') ||
    window.localStorage.getItem('enso-current-directory-id')
  if (navigatedInDrive) return false

  const homeDirQuery = {
    parentId: null,
    filterBy: null,
    labels: null,
    sortExpression: null,
    sortDirection: null,
    from: null,
    pageSize: null,
    recentProjects: false,
  }
  const onError = (err: unknown) => {
    console.error('Cannot read user home directory; will skip launching Welcome Project', err)
    return null
  }
  const homeContent = await Promise.all([
    localBackend?.listDirectory(homeDirQuery),
    onlineManager.isOnline() && remoteBackend != null ?
      remoteBackend.listDirectory(homeDirQuery, 'User Home')
    : null,
  ]).catch(onError)
  if (homeContent == null) return false
  const [localHome, cloudHome] = homeContent
  return ![...(localHome?.assets ?? []), ...(cloudHome?.assets ?? [])].some((asset) => {
    return asset.type != AssetType.directory || asset.title != SAMPLES_DIRECTORY
  })
}

async function uploadProjectArchive(
  url: string,
  localBackend: Pick<LocalBackend, 'uploadFileStart' | 'uploadFileEnd' | 'rootPath'>,
) {
  const filePath = fileURLToPath(url)
  if (filePath == null) return
  const projectName = getFileName(filePath)
  const parentDirectoryId = newDirectoryId(localBackend.rootPath())
  const metadata = await localBackend.uploadFileStart(
    {
      parentDirectoryId,
      fileName: projectName,
      fileId: null,
      filePath: Path(filePath),
    },
    null,
  )
  const endMetadata = await localBackend.uploadFileEnd({
    ...metadata,
  })
  if (endMetadata.project == null) {
    return
  }
  return endMetadata.project.ensoPath
}

/** Extract proper path from `file://` URL. */
function fileURLToPath(url: string): string | null {
  if (URL.canParse(url)) {
    const parsed = new URL(url)
    if (parsed.protocol === 'file:') {
      return decodeURIComponent(
        platform() === Platform.windows ?
          // On Windows, we must remove leading `/` from URL.
          parsed.pathname.slice(1)
        : parsed.pathname,
      )
    } else {
      return null
    }
  } else {
    return null
  }
}

/** If routing to a view assigned to some tab, open this tab. */
export async function openTab(to: RouteLocation) {
  switch (to.name) {
    case 'project': {
      const container = useContainerData()
      if (typeof to.params.id !== 'string' || !isProjectId(to.params.id)) return false
      const id = to.params.id
      const tab: Tab = { type: 'project', id }
      if (container.isTabOpened(tab)) {
        break
      }
      const { localBackend, remoteBackend } = useBackends()
      const queryClient = useQueryClient()

      const backend = isLocalProjectId(id) ? localBackend : remoteBackend
      if (backend == null) return false

      const options = backendQueryOptions('getAssetDetails', [id, undefined], backend)
      const assetResponse: AssetDetailsResponse<ProjectId> = await queryClient.fetchQuery(options)
      if (!assetResponse) return false

      container.openProjectLocally(assetResponse, backend.type, false)
      break
    }
    case 'settings': {
      const container = useContainerData()
      container.openSettingsTab(false)
    }
  }
}
