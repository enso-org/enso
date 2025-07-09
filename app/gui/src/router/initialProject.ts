import { Plan, User } from '#/services/Backend'
import LocalBackend from '#/services/LocalBackend'
import LocalStorage from '#/utilities/LocalStorage'
import { useAuth, UserSessionType } from '$/providers/auth'
import { useBackends } from '$/providers/backends'
import { ensoPathToTabId } from '$/providers/container'
import { injectGuiConfig } from '@/providers/guiConfig'
import { RouteLocation } from 'vue-router'

export const LOCAL_INITIAL_PROJECT_RELATIVE_PATH = 'Samples/Getting_Started_Reading'
export const CLOUD_INITIAL_PROJECT_RELATIVE_PATH = 'Samples/Colorado COVID.project'

type Backend = Pick<LocalBackend, 'rootPath'>

/** Get path of the project to auto-open on application launch. */
export function initialProjectPath(
  cliStartupProject: string | undefined,
  user: User,
  { localBackend }: { localBackend: Backend | null } = useBackends(),
) {
  let path: string | undefined

  if (cliStartupProject?.startsWith('file:')) return undefined
  // If not file url, we expect this parameter to be a project name
  if (cliStartupProject) {
    path = `${localBackend?.rootPath()}/${cliStartupProject}`
  } else {
    const navigatedInDrive =
      window.localStorage.getItem('enso-category-id') ||
      window.localStorage.getItem('enso-current-directory-id')
    if (!navigatedInDrive && !LocalStorage.getInstance().get('launchedProjects')) {
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
  if (auth.session?.type !== UserSessionType.full) return

  const initialPath = initialProjectPath(config.params.startup.project, auth.session.user)
  console.debug(initialPath)
  return initialPath ? { name: 'dashboard', params: { path: initialPath.split('/') } } : true
}
