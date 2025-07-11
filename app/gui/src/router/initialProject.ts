import Backend, { AssetType, Plan, User } from '#/services/Backend'
import LocalBackend from '#/services/LocalBackend'
import RemoteBackend from '#/services/RemoteBackend'
import LocalStorage from '#/utilities/LocalStorage'
import { useAuth } from '$/providers/auth'
import { useBackends } from '$/providers/backends'
import { ensoPathToTabId } from '$/providers/container'
import { injectGuiConfig } from '@/providers/guiConfig'
import { RouteLocation } from 'vue-router'

export const SAMPLES_DIRECTORY = 'Samples'
export const LOCAL_INITIAL_PROJECT_RELATIVE_PATH = `${SAMPLES_DIRECTORY}/Getting_Started_Reading`
export const CLOUD_INITIAL_PROJECT_RELATIVE_PATH = `${SAMPLES_DIRECTORY}/Colorado COVID.project`

type BackendAPI<B extends Backend> = Pick<B, 'rootPath' | 'listDirectory'>

/** Get path of the project to auto-open on application launch. */
export async function initialProjectPath(
  cliStartupProject: string | undefined,
  user: User,
  {
    localBackend,
    remoteBackend,
  }: {
    localBackend: BackendAPI<LocalBackend> | null
    remoteBackend: BackendAPI<RemoteBackend>
  } = useBackends(),
) {
  let path: string | undefined

  if (cliStartupProject?.startsWith('file:')) return undefined
  // If not file url, we expect this parameter to be a project name
  if (cliStartupProject) {
    path = `${localBackend?.rootPath()}/${cliStartupProject}`
  } else {
    if (await shouldOpenInitialProject(localBackend, remoteBackend)) {
      if (user.plan === Plan.free) {
        path = `${localBackend?.rootPath()}/${LOCAL_INITIAL_PROJECT_RELATIVE_PATH}`
      } else {
        path = `enso://Users/${user.name}/${CLOUD_INITIAL_PROJECT_RELATIVE_PATH}`
      }
    }
  }

  if (path) {
    return ensoPathToTabId(path)
  }
}

/** A navigation guard for Dashboard page, which  */
export async function maybeRedirectToInitialProject(to: RouteLocation) {
  if (to.params.path) return

  const config = injectGuiConfig()
  const auth = useAuth()
  await auth.waitForSession()

  // In case of not being logged in, the redirection should be managed by ProtectedLayout.
  if (auth.session == null) return

  const initialPath = await initialProjectPath(config.params.startup.project, auth.session.user)
  return initialPath ? { name: 'dashboard', params: { path: initialPath.split('/') } } : true
}

async function shouldOpenInitialProject(
  localBackend: Pick<LocalBackend, 'listDirectory'> | null,
  remoteBackend: Pick<RemoteBackend, 'listDirectory'>,
) {
  const navigatedInDrive =
    window.localStorage.getItem('enso-category-id') ||
    window.localStorage.getItem('enso-current-directory-id')
  const anyProjectLaunched = LocalStorage.getInstance().get('launchedProjects')
  if (navigatedInDrive || anyProjectLaunched) return false

  const homeDirQuery = { parentId: null, filterBy: null, labels: null, recentProjects: false }
  const onError = (err: unknown) => {
    console.error('Cannot read user home directory; will skip launching Welcome Project', err)
    return null
  }

  const homeContent = await Promise.all([
    localBackend?.listDirectory(homeDirQuery) ?? [],
    remoteBackend.listDirectory(homeDirQuery, 'User Home'),
  ]).catch(onError)
  if (homeContent == null) return false
  const [localHome, cloudHome] = homeContent
  return (
    [...localHome, ...cloudHome].filter((asset) => {
      return asset.type != AssetType.directory || asset.title != SAMPLES_DIRECTORY
    }).length <= 0
  )
}
