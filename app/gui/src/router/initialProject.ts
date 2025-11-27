import { useAuth } from '$/providers/auth'
import { useBackends } from '$/providers/backends'
import { useOpenedProjects } from '$/providers/openedProjects'
import { backendQueryOptions } from '@/composables/backend'
import { injectGuiConfig } from '@/providers/guiConfig'
import { onlineManager, useQueryClient } from '@tanstack/vue-query'
import {
  AssetType,
  Backend,
  EnsoPath,
  extractTypeFromId,
  isRemoteAssetPath,
  Path,
  Plan,
  ProjectId,
  type AssetDetailsResponse,
  type User,
} from 'enso-common/src/services/Backend'
import { ensoPathEq } from 'enso-common/src/services/Backend/ensoPath'
import { newDirectoryId, type LocalBackend } from 'enso-common/src/services/LocalBackend'
import type { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { some } from 'enso-common/src/utilities/data/iter'
import { platform, Platform } from 'enso-common/src/utilities/detect'
import { getFileName } from 'enso-common/src/utilities/file'
import type { NavigationGuardReturn, RouteLocation } from 'vue-router'

export const SAMPLES_DIRECTORY = 'Samples'
export const LOCAL_WELCOME_PROJECT_RELATIVE_PATH = `${SAMPLES_DIRECTORY}/Getting_Started`
export const CLOUD_WELCOME_PROJECT_RELATIVE_PATH = `${SAMPLES_DIRECTORY}/Getting Started.project`

type BackendAPI<B extends Backend> = Pick<B, 'rootPath' | 'listDirectory'>

/** Open a project depending on path param in RounteLocation */
export async function openProjectFromPath(to: RouteLocation) {
  if (to.name !== 'dashboard' || to.params.path == null) return
  const auth = useAuth()
  const { localBackend, remoteBackend } = useBackends()
  const queryClient = useQueryClient()
  const openedProjects = useOpenedProjects()

  const path = EnsoPath(to.params.path instanceof Array ? to.params.path.join('/') : to.params.path)
  if (!path) return

  // Check if project is already opened
  if (
    some(
      openedProjects.listProjects(),
      (project) =>
        ensoPathEq(project.state.info.ensoPath, path) && project.state.status !== 'not-opened',
    )
  )
    return

  const backend = isRemoteAssetPath(path) ? remoteBackend : localBackend
  if (backend == null) return
  await auth.waitForSession()
  const resolvedPath = await backend.resolveEnsoPath(path).catch(() => null)
  const typedAsset = resolvedPath && extractTypeFromId(resolvedPath.id)
  if (typedAsset?.type !== AssetType.project) return
  const options = backendQueryOptions('getAssetDetails', [typedAsset.id, undefined], backend)
  const assetResponse: AssetDetailsResponse<ProjectId> = await queryClient.fetchQuery(options)
  if (!assetResponse) return

  openedProjects.openProjectLocally(assetResponse, backend.type)
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
  if (to.params.path) return

  const backends = useBackends()
  const config = injectGuiConfig()
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

  return initialPath ? { name: 'dashboard', params: { path: initialPath.split('/') } } : true
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
