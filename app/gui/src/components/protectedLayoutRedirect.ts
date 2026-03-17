import { ref } from 'vue'
import {
  isNavigationFailure,
  NavigationFailureType,
  type RouteLocationRaw,
  type Router,
} from 'vue-router'

type RedirectRouter = Pick<Router, 'currentRoute' | 'replace' | 'resolve'>

/**
 * Redirect scheduler for a protected layout.
 * Makes sure only one `router.replace` runs at a time.
 * If multiple redirects are issued concurrently, the last one wins.
 */
export function createProtectedLayoutRedirectController(
  router: RedirectRouter,
  onError: (error: unknown) => void,
) {
  const isRedirecting = ref(false)
  let pendingRedirect: RouteLocationRaw | undefined

  const isAtRedirectTarget = (redirectValue: RouteLocationRaw) =>
    router.resolve(redirectValue).fullPath === router.currentRoute.value.fullPath

  const flushRedirects = async () => {
    if (isRedirecting.value) {
      return
    }

    isRedirecting.value = true
    try {
      while (pendingRedirect != null) {
        const redirectValue = pendingRedirect
        pendingRedirect = undefined

        try {
          const navigationFailure = await router.replace(redirectValue)

          if (isNavigationFailure(navigationFailure, NavigationFailureType.cancelled)) {
            continue
          }

          if (navigationFailure && !isAtRedirectTarget(redirectValue)) {
            onError(navigationFailure)
          }
        } catch (error) {
          onError(error)
        }
      }
    } finally {
      isRedirecting.value = false

      if (pendingRedirect != null) {
        void flushRedirects()
      }
    }
  }

  const redirectTo = (redirectValue: RouteLocationRaw) => {
    pendingRedirect = redirectValue
    void flushRedirects()
  }

  return { redirectTo }
}
