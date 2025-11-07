/**
 * @file Definition of the Electron-specific parts of the authentication flows of the IDE.
 *
 * # Overview of Authentication/Authorization
 *
 * Actions like creating projects, opening projects, uploading files to the cloud, etc. require the
 * user to be authenticated and authorized. Authenticated means that the user has an account that
 * the application recognizes, and that the user has provided their credentials to prove that they
 * are who they say they are. Authorized means that the user has the necessary permissions to
 * perform the action.
 *
 * Authentication and authorization are provided by the user logging in with their credentials,
 * which we exchange for a JSON Web Token (JWT). The JWT is sent with every HTTP request to the
 * backend.
 *
 * The authentication module of the dashboard and IDE handles these flows:
 * - registering a new user account,
 * - signing in to an existing user account (in exchange for an access token),
 * - signing out of the user account,
 * - setting the user's username (i.e., display name used in place of their email address),
 * - changing/resetting the user's password,
 * - etc.
 *
 * # Electron Inter-Process Communication (IPC)
 *
 * If the user is signing in through a federated identity provider (e.g., Google or GitHub), the
 * authentication flows need be able to to:
 * - redirect the user from the IDE to external sources (e.g., system web browser), and
 * - redirect the user from external sources to the IDE (e.g., system web browser, email client).
 *
 * The main Electron process can launch the system web browser. The dashboard and IDE are sandboxed,
 * so they can not launch the system web browser. By registering Inter-Process Communication (IPC)
 * listeners in the Electron app, we can bridge this gap, and allow the dashboad + IDE to emit
 * events that signal to the main Electron process to open URLs in the system web browser.
 *
 * ## Redirect To System Web Browser
 *
 * The user must use the system browser to complete sensitive flows such as signup and signin. These
 * flows should not be done in the app as the user cannot be expected to trust the app with their
 * credentials.
 *
 * To redirect the user from the IDE to an external source:
 * 1. Register a listener for {@link Channel.openUrlInSystemBrowser} IPC events.
 * 2. Emit an {@link Channel.openUrlInSystemBrowser} event. The listener registered in step
 * 1 will use the {@link opener} library to open the event's {@link URL}
 * argument in the system web browser, in a cross-platform way.
 *
 * ## Redirect To IDE
 *
 * The user must be redirected back to the IDE from the system web browser after completing a
 * sensitive flow such as signup or signin. The user may also be redirected to the IDE from an
 * external source such as an email client after verifying their email address.
 *
 * To handle these redirects, we use deep links. Deep links are URLs that are used to redirect the
 * user to a specific page in the application. To handle deep links, we use a custom URL protocol
 * scheme.
 *
 * To prepare the application to handle deep links:
 * - Register a custom URL protocol scheme with the OS (c.f., `electron-builder-config.ts`).
 * - Define a listener for Electron `OPEN_URL_EVENT`s.
 * - Define a listener for {@link Channel.openDeepLink} events (c.f., `preload.ts`).
 *
 * Then when the user clicks on a deep link from an external source to the IDE:
 * - The OS redirects the user to the application.
 * - The application emits an Electron `OPEN_URL_EVENT`.
 * - The `OPEN_URL_EVENT` listener checks if the {@link URL} is a deep link.
 * - If the {@link URL} is a deep link, the `OPEN_URL_EVENT` listener prevents Electron from
 * handling the event.
 * - The `OPEN_URL_EVENT` listener then emits an {@link Channel.openDeepLink} event.
 * - The {@link Channel.openDeepLink} listener registered by the dashboard receives the event.
 * Then it parses the {@link URL} from the event's {@link URL} argument. Then it uses the
 * {@link URL} to redirect the user to the dashboard, to the page specified in the {@link URL}'s
 * `pathname`.
 */
import { CREDENTIALS_PATH } from '@/paths'
import {
  createAWSCredentialsAndIdentityIdProvider,
  createKeyValueStorageFromCookieStorageAdapter,
  createUserPoolsTokenProvider,
  runWithAmplifyServerContext,
} from 'aws-amplify/adapter-core'
import { getCurrentUser } from 'aws-amplify/auth/server'
import type { BrowserWindow } from 'electron'
import type { AccessToken } from 'enso-common/src/accessToken'
import { DEEP_LINK_SCHEME } from 'enso-common/src/constants'
import { mkdir, readFileSync, unlinkSync, writeFile } from 'node:fs'
import { dirname } from 'node:path'
import opener from 'opener'
import type { Electron } from './electron.js'
import { Channel } from './ipc.js'
import { registerUrlCallback } from './urlAssociations.js'

/**
 * Configure all the functionality that must be set up in the Electron app to support
 * authentication-related flows. Must be called in the Electron app `whenReady` event.
 * @param window - A function that returns the main Electron window. This argument is a lambda and
 * not a variable because the main window is not available when this function is called. This module
 * does not use the `window` until after it is initialized, so while the lambda may return `null` in
 * theory, it never will in practice.
 */
export function initAuthentication(electron: Electron, window: () => BrowserWindow) {
  // Listen for events to open a URL externally in a browser the user trusts. This is used for
  // OAuth authentication, both for trustworthiness and for convenience (the ability to use the
  // browser's saved passwords).
  electron.ipcMain.on(Channel.openUrlInSystemBrowser, (_event, url: string) => {
    console.log(`Opening URL '${url}' in the default browser.`)
    opener(url)
  })

  // Listen for events to handle deep links.
  registerUrlCallback(electron, (url) => {
    console.log(`Received 'open-url' event for '${url.toString()}'.`)
    if (url.protocol !== `${DEEP_LINK_SCHEME}:`) {
      console.error(`'${url.toString()}' is not a deep link, ignoring.`)
    } else {
      console.log(`'${url.toString()}' is a deep link, sending to renderer.`)
      window().webContents.send(Channel.openDeepLink, url.toString())
    }
  })

  // Listen for events to save the given user credentials to `~/.enso/credentials`.
  electron.ipcMain.on(Channel.saveAccessToken, (event, accessTokenPayload: AccessToken | null) => {
    event.preventDefault()

    if (accessTokenPayload == null) {
      try {
        unlinkSync(CREDENTIALS_PATH)
      } catch {
        // Ignored, most likely the path does not exist.
      }
    } else {
      mkdir(dirname(CREDENTIALS_PATH), { recursive: true }, (error) => {
        if (error) {
          console.error(`Could not create '${dirname(CREDENTIALS_PATH)}' directory.`)
        } else {
          writeFile(
            CREDENTIALS_PATH,
            JSON.stringify({
              /* eslint-disable camelcase */
              client_id: accessTokenPayload.clientId,
              access_token: accessTokenPayload.accessToken,
              refresh_token: accessTokenPayload.refreshToken,
              refresh_url: accessTokenPayload.refreshUrl,
              expire_at: accessTokenPayload.expireAt,
              /* eslint-enable camelcase */
            }),
            (innerError) => {
              if (innerError) {
                console.error(`Could not write to the credentials file at '${CREDENTIALS_PATH}'.`)
              }
            },
          )
        }
      })
    }
  })
}

/** Read the access token stored in the credentials file. */
export function readAccessToken(): AccessToken | undefined {
  try {
    return JSON.parse(readFileSync(CREDENTIALS_PATH, { encoding: 'utf-8' }))
  } catch {
    return
  }
}

/**
 *
 */
export async function getUpToDateAccessToken(): Promise<string> {
  const accessToken = readAccessToken()
  if (!accessToken) {
    throw new Error('No access token found for refreshing.')
  }
  // Create the key-value storage from Remix's cookie API
  const keyValueStorage = createKeyValueStorageFromCookieStorageAdapter({
    get(name) {
      const encodedName = ensureEncodedForJSCookie(name)
      const cookieRegex = new RegExp(`(^|;)\\s*${encodedName}=([^;]+)`)
      const match = cookies.match(cookieRegex)
      const cookie = match ? { name, value: match[2] } : undefined

      if (cookie && name.endsWith('.signInDetails')) {
        cookie.value = decodeURIComponent(cookie.value)
      }

      return cookie
    },
    getAll() {
      return cookies.split('; ').map((cookie) => {
        const [name, value] = cookie.split('=')
        return { name, value }
      })
    },
    set(name, value) {
      // Not needed on the server unless setting cookies
    },
    delete(name) {
      // Not needed on the server unless deleting cookies
    },
  })

  // Create the tokenProvider
  const tokenProvider = createUserPoolsTokenProvider(authConfig, keyValueStorage)
  // Create the credentialsProvider
  const credentialsProvider = createAWSCredentialsAndIdentityIdProvider(authConfig, keyValueStorage)
  const what = await runWithAmplifyServerContext(
    { Auth: authConfig },
    { Auth: { tokenProvider, credentialsProvider }, ssr: true },
    async (spec) => {
      try {
        return await getCurrentUser(spec)
      } catch (error) {
        console.error('Error fetching authentication session:', error)
        return null
      }
    },
  )
  return accessToken.accessToken
  // TODO: Implement the logic to refresh the access token using the refresh token.
}
