import BackendFull, { Plan, User } from '#/services/Backend'
import LocalStorage from '#/utilities/LocalStorage'
import { useAuth, UserSessionType } from '$/providers/auth'
import { useBackends } from '$/providers/backends'
import { ensoPathToTabId } from '$/providers/container'
import { injectGuiConfig } from '@/providers/guiConfig'
import { RouteLocation } from 'vue-router'
import { assert } from 'ydoc-shared/util/assert'

export const LOCAL_INITIAL_PROJECT_RELATIVE_PATH = 'Samples/Getting_Started_Reading'
export const CLOUD_INITIAL_PROJECT_RELATIVE_PATH = 'Samples/Colorado COVID.project'

type Backend = Pick<BackendFull, 'rootPath'>

/** Get path of the project to auto-open on application launch. */
export function initialProjectPath(
  cliStartupProject: string | undefined,
  user: User,
  {
    localBackend,
    remoteBackend,
  }: { localBackend: Backend | null; remoteBackend: Backend } = useBackends(),
) {
  let relativePath: string | undefined
  let backend: Backend | null | undefined

  if (cliStartupProject?.startsWith('file:')) return undefined
  // If not file url, we expect this parameter to be a project name
  if (cliStartupProject) {
    relativePath = cliStartupProject
    backend = localBackend
  } else {
    const navigatedInDrive =
      window.localStorage.getItem('enso-category-id') ||
      window.localStorage.getItem('enso-current-directory-id')
    if (!navigatedInDrive && !LocalStorage.getInstance().get('launchedProjects')) {
      if (user.plan === Plan.free) {
        relativePath = LOCAL_INITIAL_PROJECT_RELATIVE_PATH
        backend = localBackend
      } else {
        relativePath = CLOUD_INITIAL_PROJECT_RELATIVE_PATH
        backend = remoteBackend
      }
    }
  }

  if (relativePath && backend) {
    return ensoPathToTabId(`${backend.rootPath(user)}/${relativePath}`)
  }
}

/** A navigation guard for Dashboard page, which  */
export async function maybeRedirectToInitialProject(to: RouteLocation) {
  if (to.params.path) return

  const config = injectGuiConfig()
  const auth = useAuth()
  await auth.waitForSession()

  assert(auth.session?.type === UserSessionType.full)

  const initialPath = initialProjectPath(config.params.startup.project, auth.session.user)
  return initialPath ? { params: { path: initialPath.split('/') } } : true
}
