/** @file Definition of the Electron-specific parts of the authentication flows of the IDE.
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
 * `openExternalUrl` IPC events are used to open URLs in the user's system browser.
 *
 * The user must use the system browser to complete sensitive flows such as signup and signin. These
 * flows should not be done in the app as the user cannot be expected to trust the app with their
 * credentials.
 *
 * To redirect the user from the IDE to an external source:
 * 1. Call the {@link initIpc} function to register a listener for
 * {@link ipc.channel.openExternalUrl} IPC events.
 * 2. Emit an {@link ipc.channel.openExternalUrl} event. The listener registered in the
 * {@link initIpc} function will use the {@link opener} library to open the event's {@link URL}
 * argument in the system web browser, in a cross-platform way.
 * 
 * ## Redirect To IDE
 *
 * The user must be redirected back to the IDE from the system web browser after completing a
 * sensitive flow such as signup or signin. The user may also be redirected to the IDE from an
 * external source such as an email client after verifying their email address.
 *
 * To redirect the user from external sources to the IDE:
 * 1. Call the {@link exposeAuthenticationApi} function in `preload.ts` to register a listener for
 * {@link ipc.channel.openAuthenticationUrl} IPC events.
 * 2. Define a URL protocol scheme in `electron-builder-config.ts`. Electron will register it with
 * the OS.
 * 3. Use this URL protocol scheme in links to create deep links.
 * 4. When the user clicks on a deep link, the OS will redirect the user to IDE.
 * 5. The IDE will receive a {@link ipc.channel.openAuthenticationUrl} event. The listener
 * registered in the {@link exposeAuthenticationApi} function will parse the URL from the event's
 * {@link URL} argument, and use it to redirect the user to the correct page in the IDE. */
import * as electron from 'electron'
import * as ipc from 'ipc'
import * as shared from '../shared'
import opener from 'opener'



// =================
// === Constants ===
// =================

/** Base URL path that all URLs handled by the {@link initOpenUrlListener} function must be prefixed
 * with.
 *
 * Note the double leading slash on the pathname. This is because we use a custom URL protocol
 * scheme for these URLs. When parsing strings as URLs, the `url.pathname` is prefixed with an extra
 * slash. For example, `new URL('enso://authentication/register').pathname` results in
 * `//authentication/register`. */
const AUTHENTICATION_PATHNAME_BASE = "//authentication"

export const IPC_CHANNELS = {
    /** Channel for requesting that a URL by opened by the system browser. */
    openExternalUrl: 'open-external-url',
    /** Channel for setting a callback to handle deep links to this application */
    setOpenAuthenticationUrlCallback: 'set-open-authentication-url-callback',
    /** Channel for requesting that a URL by opened by the Electron app. */
    openAuthenticationUrl: 'open-authentication-url',
}

/** API object exposed to the dashboard and IDE. Contains methods that can be used to open URLs in
 * the system browser. */
const AUTHENTICATION_API_KEY = 'authenticationApi'
/** Name of the Electron event that is emitted when a URL is opened in Electron (e.g., when the user
 * clicks a link in the dashboard). */
const OPEN_URL_EVENT = 'open-url'



// =================================
// === Expose Authentication API ===
// =================================

/** Exposes an `AuthenticationApi` object on the main window that can be used from within our
 * dashboard to open OAuth flows in the system browser.
 * 
 * This must be done because the dashboard application is sandboxed and thus not privileged to
 * do so unless we explicitly expose this functionality. */
export const exposeAuthenticationApi = () => {
    electron.contextBridge.exposeInMainWorld(AUTHENTICATION_API_KEY, {
        /** Open a URL in the system browser (rather than in the app). */
        openExternalUrl: (url: string) => electron.ipcRenderer.send(ipc.channel.openExternalUrl, url),
        /** Set the callback that will be called when an authenticated-related URL is opened.
         *
         * The callback is intended to handle links like
         * `enso://authentication/register?code=...&state=...` from external sources like the user's
         * system browser or email client. Handling the links involves resuming whatever flow was in
         * progress when the link was opened (e.g., an OAuth registration flow). */
        setOpenAuthenticationUrlCallback: (callback: (url: string) => void) =>
            electron.ipcRenderer.on(ipc.channel.openAuthenticationUrl, (_event, url) => callback(url)),
    })
}



// ========================================
// === Initialize Authentication Module ===
// ========================================

/** Configures all the functionality that must be set up in the Electron app to support
 * authentication-related flows. Must be called in the Electron app `whenReady` event. */
export const initAuthenticationModule = (
    window: () => electron.BrowserWindow | null,
) => {
    initIpc()
    initOpenUrlListener(window)
}

/** Registers an Inter-Process Communication (IPC) channel between the Electron application and the
 * served website.
 * 
 * This channel listens for `openExternalUrl` events. When an `openExternalEvent` is fired, this
 * listener will assume that the first and only argument of the event is a URL. This listener will
 * then attempt to open the URL in a cross-platform way. The intent is to open the URL in the system
 * browser.
 * 
 * This functionality is necessary because we don't want to run the OAuth flow in the app. Users
 * don't trust Electron apps to handle their credentials. */
const initIpc = () => {
    electron.ipcMain.on(ipc.channel.openExternalUrl, (_event, url) => opener(url))
}

/** Initialize the listener for `open-url` events.
 *
 * This listener is used to open a page in *this* application window, when the user is
 * redirected to a URL with a protocol supported by this application.
 *
 * For example, when the user completes an OAuth sign in flow (e.g., through Google), they are
 * redirected to a URL like `enso://authentication/register?code=...`. This listener will intercept
 * that URL and open the page `register?code=...` in the application window.
 * 
 * All URLs that aren't deep links (i.e., URLs that don't use the {@link shared.DEEP_LINK_SCHEME}
 * protocol) will be ignored by this handler. All URLs that don't have a pathname that starts with
 * {@link AUTHENTICATION_PATHNAME_BASE} will be ignored by this handler. */
const initOpenUrlListener = (
    window: () => electron.BrowserWindow | null,
) => {
    electron.app.on(OPEN_URL_EVENT, (event, url) => {
        const parsedUrl = new URL(url)

        if (parsedUrl.protocol !== `${shared.DEEP_LINK_SCHEME}:`) {
            return
        }

        if (!parsedUrl.pathname.startsWith(AUTHENTICATION_PATHNAME_BASE)) {
            return
        }

        /** Don't open the deep link URL in the window, we want the system browser to handle it. */
        event.preventDefault()

        const pathname = parsedUrl.pathname
        const search = parsedUrl.search
        const href = `${pathname}${search}`
        window()?.webContents.send(ipc.channel.openAuthenticationUrl, href)
    })
}
