/**
 * @file Provides an {@link AuthService} which consists of an underyling `Cognito` API
 * wrapper, along with some convenience callbacks to make URL redirects for the authentication flows
 * work with Electron.
 */
import * as appUtils from '$/appUtils'
import { useRouter } from 'vue-router'

const router = useRouter()

/**
 * Set the callback that will be invoked when a deep link to the application is opened.
 *
 * Typically this callback is invoked when the user is redirected back to the app after:
 *
 * 1. Authenticating with a federated identity provider; or
 * 2. Clicking a "reset password" link in a password reset email.
 *
 * For example, when the user completes an OAuth sign in flow (e.g., through Google), they are
 * redirected to a URL like `enso://authentication/register?code=...`. This listener will intercept
 * that URL and open the page `register?code=...` in the application window.
 *
 * This is only used when running on the desktop, as the browser version of the app lets Amplify
 * handle the redirect for us. On the desktop however, we need to handle the redirect ourselves,
 * because it's a deep link into the app, and Amplify doesn't handle deep links.
 *
 * All URLs that don't have a pathname that starts with `AUTHENTICATION_PATHNAME_BASE` will be
 * ignored by this handler.
 */
window.authenticationApi?.setDeepLinkHandler((urlString: string) => {
  const url = new URL(urlString)
  console.log(`Parsed pathname: ${url.pathname}`)
  // Remove the trailing slash in the pathname - it is present on Windows but not on macOS.
  const pathname = url.pathname.replace(/\/$/, '')
  switch (pathname) {
    case '//auth': {
      if (url.search === '') {
        // Signing out.
        void router.push(appUtils.LOGIN_PATH)
      } else {
        // Signing in.
        void router.push(appUtils.DASHBOARD_PATH)
      }
      break
    }
    case '//payments/success': {
      void router.push(`${appUtils.PAYMENTS_SUCCESS_PATH}${url.search}`)
      break
    }
    default: {
      void router.push(pathname.slice(1))
      break
    }
  }
})
