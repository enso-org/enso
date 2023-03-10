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
 * The user must use the system browser to complete sensitive flows such as signup and signin. These
 * flows should not be done in the app as the user cannot be expected to trust the app with their
 * credentials.
 *
 * To redirect the user from the IDE to an external source:
 * 1. Call the {@link initIpc} function to register a listener for
 * {@link ipc.channel.openUrlInSystemBrowser} IPC events.
 * 2. Emit an {@link ipc.channel.openUrlInSystemBrowser} event. The listener registered in the
 * {@link initIpc} function will use the {@link opener} library to open the event's {@link URL}
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
 * - Define a listener for Electron {@link OPEN_URL_EVENT}s (c.f., {@link initOpenUrlListener}).
 * - Define a listener for {@link ipc.channel.openDeepLink} events (c.f., `preload.ts`).
 *
 * Then when the user clicks on a deep link from an external source to the IDE:
 * - The OS redirects the user to the application.
 * - The application emits an Electron {@link OPEN_URL_EVENT}.
 * - The {@link OPEN_URL_EVENT} listener checks if the {@link URL} is a deep link.
 * - If the {@link URL} is a deep link, the {@link OPEN_URL_EVENT} listener prevents Electron from
 * handling the event.
 * - The {@link OPEN_URL_EVENT} listener then emits an {@link ipc.channel.openDeepLink} event.
 * - The {@link ipc.channel.openDeepLink} listener registered by the dashboard receives the event.
 * Then it parses the {@link URL} from the event's {@link URL} argument. Then it uses the
 * {@link URL} to redirect the user to the dashboard, to the page specified in the {@link URL}'s
 * `pathname`. */

import * as content from 'enso-content-config'
import * as electron from 'electron'
import * as ipc from 'ipc'
import * as shared from '../shared'
import * as childProcess from 'child_process'
import * as os from 'os'

// =================
// === Constants ===
// =================

/** Name of the Electron event that is emitted when a URL is opened in Electron (e.g., when the user
 * clicks a link in the dashboard). */
const OPEN_URL_EVENT = 'open-url'

// ========================================
// === Initialize Authentication Module ===
// ========================================

/** Configures all the functionality that must be set up in the Electron app to support
 * authentication-related flows. Must be called in the Electron app `whenReady` event.
 *
 * @param window - A function that returns the main Electron window. This argument is a lambda and
 * not a variable because the main window is not available when this function is called. This module
 * does not use the `window` until after it is initialized, so while the lambda may return `null` in
 * theory, it never will in practice. */
export const initModule = (window: () => electron.BrowserWindow) => {
    initIpc()
    initOpenUrlListener(window)
}

/** Registers an Inter-Process Communication (IPC) channel between the Electron application and the
 * served website.
 *
 * This channel listens for {@link ipc.channel.openUrlInSystemBrowser} events. When this kind of
 * event is fired, this listener will assume that the first and only argument of the event is a URL.
 * This listener will then attempt to open the URL in a cross-platform way. The intent is to open
 * the URL in the system browser.
 *
 * This functionality is necessary because we don't want to run the OAuth flow in the app. Users
 * don't trust Electron apps to handle their credentials. */
const initIpc = () => {
    electron.ipcMain.on(ipc.channel.openUrlInSystemBrowser, (_event, url) => opener(url))
}

/** Registers a listener that fires a callback for `open-url` events, when the URL is a deep link.
 *
 * This listener is used to open a page in *this* application window, when the user is
 * redirected to a URL with a protocol supported by this application.
 *
 * All URLs that aren't deep links (i.e., URLs that don't use the {@link shared.DEEP_LINK_SCHEME}
 * protocol) will be ignored by this handler. Non-deep link URLs will be handled by Electron. */
const initOpenUrlListener = (window: () => electron.BrowserWindow) => {
    electron.app.on(OPEN_URL_EVENT, (event, url) => {
        const parsedUrl = new URL(url)
        /** Prevent Electron from handling the URL at all, because redirects can be dangerous. */
        event.preventDefault()
        if (parsedUrl.protocol !== `${shared.DEEP_LINK_SCHEME}:`) {
            content.logger.error(`${url} is not a deep link, ignoring.`)
            return
        }
        window().webContents.send(ipc.channel.openDeepLink, url)
    })
}

// ==============
// === Opener ===
// ==============

/** Function for opening URLs in the system browser in a cross-platform way.
 *
 * This function contains the contents of the `opener` NPM package, as of commit
 * https://github.com/domenic/opener/commit/24edf48a38d1e23bbc5ffbeb079c206d5565f062.  To avoid an
 * extra external dependency, the contents of the function have been copied into this file. The
 * function has been modified to follow the Enso style guide, and to be TypeScript-compatible. */
const opener = (args: any, options?: any, callback?: any) => {
    let platform = process.platform

    /** Attempt to detect Windows Subystem for Linux (WSL). WSL  itself as Linux (which works in
     * most cases), but in this specific case we need to treat it as actually being Windows. The
     * "Windows-way" of opening things through cmd.exe works just fine here, whereas using xdg-open
     * does not, since there is no X Windows in WSL. */
    if (platform === 'linux' && os.release().indexOf('Microsoft') !== -1) {
        platform = 'win32'
    }

    /** http://stackoverflow.com/q/1480971/3191, but see below for Windows. */
    let command
    switch (platform) {
        case 'win32': {
            command = 'cmd.exe'
            break
        }
        case 'darwin': {
            command = 'open'
            break
        }
        default: {
            command = 'xdg-open'
            break
        }
    }

    if (typeof args === 'string') {
        args = [args]
    }

    if (typeof options === 'function') {
        callback = options
        options = {}
    }

    if (options && typeof options === 'object' && options.command) {
        if (platform === 'win32') {
            /* *always* use cmd on windows */
            args = [options.command].concat(args)
        } else {
            command = options.command
        }
    }

    if (platform === 'win32') {
        /** On Windows, we really want to use the "start" command. But, the rules regarding
         * arguments with spaces, and escaping them with quotes, can get really arcane. So the
         * easiest way to deal with this is to pass off the responsibility to "cmd /c", which has
         * that logic built in.
         *
         * Furthermore, if "cmd /c" double-quoted the first parameter, then "start" will interpret
         * it as a window title, so we need to add a dummy empty-string window title:
         * http://stackoverflow.com/a/154090/3191
         *
         * Additionally, on Windows ampersand and caret need to be escaped when passed to "start" */
        args = args.map((value: any) => {
            return value.replace(/[&^]/g, '^$&')
        })
        args = ['/c', 'start', '""'].concat(args)
    }

    return childProcess.execFile(command, args, options, callback)
}
