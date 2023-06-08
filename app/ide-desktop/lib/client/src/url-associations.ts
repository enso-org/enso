/** @file URL associations for the IDE. */

import * as electron from 'electron'
import electronIsDev from 'electron-is-dev'

import * as common from 'enso-common'
import * as contentConfig from 'enso-content-config'

const logger = contentConfig.logger

// ============================
// === Protocol Association ===
// ============================

/** Register the application as a handler for our [deep link scheme]{@link common.DEEP_LINK_SCHEME}.
 *
 * This method is no-op when used under the Electron dev mode, as it requires special handling to
 * set up the process.
 *
 * It is also no-op on macOS, as the OS handles the URL opening by passing the `open-url` event to
 * the application, thanks to the information baked in our application by `electron-builder`. */
export function registerAssociations() {
    if (!electron.app.isDefaultProtocolClient(common.DEEP_LINK_SCHEME)) {
        if (electronIsDev) {
            logger.log('Not registering protocol client in dev mode.')
        } else if (process.platform === 'darwin') {
            // Registration is handled automatically there thanks to electron-builder.
            logger.log('Not registering protocol client on macOS.')
        } else {
            logger.log('Registering protocol client.')
            electron.app.setAsDefaultProtocolClient(common.DEEP_LINK_SCHEME)
        }
    } else {
        logger.log('Protocol client already registered.')
    }
}

// ====================
// === URL handling ===
// ====================

/** Check if the given list of application startup arguments denotes an attempt to open a URL.
 *
 * For example, this happens on Windows when the browser redirects user using our
 * [deep link scheme]{@link common.DEEP_LINK_SCHEME}. On macOS this is not used, as the OS
 * handles the URL opening by passing the `open-url` event to the application.
 *
 * @param clientArgs - A list of arguments passed to the application, stripped from the initial
 * executable name and any electron dev mode arguments.
 * @returns The URL to open, or `null` if no file was specified. */
export function argsDenoteUrlOpenAttempt(clientArgs: string[]): URL | null {
    const arg = clientArgs[0]
    let result: URL | null = null
    logger.log(`Checking if '${clientArgs.toString()}' denotes a URL to open.`)
    // Check if the first argument parses as a URL using our deep link scheme.
    if (clientArgs.length === 1 && typeof arg !== 'undefined') {
        try {
            const url = new URL(arg)
            logger.log(`Parsed '${arg}' as URL: ${url.toString()}. Protocol: ${url.protocol}.`)
            if (url.protocol === `${common.DEEP_LINK_SCHEME}:`) {
                result = url
            }
        } catch (e) {
            logger.log(`The single argument '${arg}' does not denote a valid URL: ${String(e)}`)
        }
    }
    return result
}

/** Handle the case where IDE is invoked with a URL to open.
 *
 * This happens on Windows when the browser redirects user using the deep link scheme.
 *
 * @param openedUrl - The URL to open. */
export function handleOpenUrl(openedUrl: URL) {
    logger.log(`Opening URL '${openedUrl.toString()}'.`)
    const appLock = electron.app.requestSingleInstanceLock({ openedUrl })
    if (!appLock) {
        // If we failed to acquire the lock, it means that another instance of the application is
        // already running. In this case, we must send the URL to the existing instance and exit.
        logger.log('Another instance of the application is already running. Exiting.')
        // Note that we need here to exit rather than quit. Otherwise, the application would
        // continue initializing and would create a new window, before quitting.
        // We don't want anything to flash on the screen, so we just exit.
        electron.app.exit(0)
    } else {
        // If we acquired the lock, it means that we are the first instance of the application.
        // In this case, we must wait for the application to be ready and then send the URL to the
        // renderer process.
        // If we supported starting the application from the URL, we should add this logic here.
        // However, we currently only use our custom URL scheme to handle authentication, so we
        // don't need to do anything here.
        logger.log('We are the first instance of the application. This is not expected.')
    }
}

/** Register the callback that will be called when the application is requested to open a URL.
 *
 * This method serves to unify the url handling between macOS and Windows. On macOS, the OS
 * handles the URL opening by passing the `open-url` event to the application. On Windows, a
 * new instance of the application is started and the URL is passed as a command line argument.
 *
 * This method registers the callback for both events. Note that on Windows it is necessary to
 * use {@link setAsUrlHandler} and {@link unsetAsUrlHandler} to ensure that the callback
 * is called.
 *
 * @param callback - The callback to call when the application is requested to open a URL. */
export function registerUrlCallback(callback: (url: URL) => void) {
    // First, register the callback for the `open-url` event. This is used on macOS.
    electron.app.on('open-url', (event, url) => {
        logger.log(`Got URL from 'open-url' event: '${url}'.`)
        event.preventDefault()
        callback(new URL(url))
    })

    // Second, register the callback for the `second-instance` event. This is used on Windows.
    electron.app.on('second-instance', (event, argv) => {
        logger.log(`Got data from 'second-instance' event: '${argv.toString()}'.`)
        unsetAsUrlHandler()
        // Check if additional data is an object that contains the URL.
        const requestOneLastElementSlice = -1
        const lastArgumentSlice = argv.slice(requestOneLastElementSlice)
        const url = argsDenoteUrlOpenAttempt(lastArgumentSlice)
        if (url) {
            logger.log(`Got URL from 'second-instance' event: '${url.toString()}'.`)
            // Even we received the URL, our Window likely is not in the foreground - the focus
            // went to the "second instance" of the application. We must bring our Window to the
            // foreground, so the user gets back to the IDE after the authentication.
            const primaryWindow = electron.BrowserWindow.getAllWindows()[0]
            if (primaryWindow) {
                if (primaryWindow.isMinimized()) {
                    primaryWindow.restore()
                }
                primaryWindow.focus()
            } else {
                logger.error('No primary window found after receiving URL from second instance.')
            }
            logger.log(`Got URL from second instance: '${url.toString()}'.`)
            event.preventDefault()
            callback(url)
        }
    })
}

// ===============================
// === Temporary handler setup ===
// ===============================

/** Make this application instance the recipient of URL callbacks.
 *
 * After the callback is received (or no longer expected), the `urlCallbackCompleted` function
 * must be called. Otherwise, other IDE instances will not be able to receive their URL
 * callbacks.
 *
 * The mechanism is built on top of the Electron's
 * [instance lock]{@link https://www.electronjs.org/docs/api/app#apprequestsingleinstancelock}
 * functionality.
 *
 * @throws {Error} An error if another instance of the application has already acquired the lock. */
export function setAsUrlHandler() {
    logger.log('Expecting URL callback, acquiring the lock.')
    if (!electron.app.requestSingleInstanceLock()) {
        const message = 'Another instance of the application is already running. Exiting.'
        logger.error(message)
        // eslint-disable-next-line no-restricted-syntax
        throw new Error(message)
    }
}

/** Stop this application instance from receiving URL callbacks.
 *
 * This function releases the instance lock that was acquired by the {@link setAsUrlHandler}
 * function. This is necessary to ensure that other IDE instances can receive their
 * URL callbacks. */
export function unsetAsUrlHandler() {
    logger.log('URL callback completed, releasing the lock.')
    electron.app.releaseSingleInstanceLock()
}
