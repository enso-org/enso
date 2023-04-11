/** @file URL associations for the IDE. */

import * as electron from "electron";
import electronIsDev from "electron-is-dev";

import * as common from 'enso-common'
import * as contentConfig from 'enso-content-config'


const logger = contentConfig.logger


export function registerAssociations() {
    if(!electron.app.isDefaultProtocolClient(common.DEEP_LINK_SCHEME)) {
        if(electronIsDev) {
            logger.log('Not registering protocol client in dev mode.')
        } else if(process.platform === 'darwin') {
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


/**
 * Check if the given list of application startup arguments denotes an attempt to open a URL.
 *
 * For example, this happens on Windows when the browser redirects user using our
 * [deep link scheme]{@link common.DEEP_LINK_SCHEME}. On macOS this is not used, as the OS
 * handles the URL opening by passing the `open-url` event to the application.
 *
 * @param clientArgs - A list of arguments passed to the application, stripped from the initial
 * executable name and any electron dev mode arguments.
 * @returns The URL to open, or `null` if no file was specified.
 */
export function argsDenoteUrlOpenAttempt(clientArgs: string[]): URL | null {
    const arg = clientArgs[0]
    let result: URL | null = null
    logger.log(`Checking if '${clientArgs}' denote a URL to open.`)
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
 * @param openedUrl - The URL to open.
 */
export function handleOpenUrl(openedUrl: URL) {
    logger.log(`Opening URL '${openedUrl.toString()}'.`)
    const appLock = electron.app.requestSingleInstanceLock({openedUrl})
    if (!appLock) {
        // If we failed to acquire the lock, it means that another instance of the application is
        // already running. In this case, we must send the URL to the existing instance and exit.
        logger.log('Another instance of the application is already running. Exiting.')
        electron.app.quit()
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

export function registerUrlCallback(callback: (url: URL) => void) {
    // First, register the callback for the `open-url` event. This is used on macOS.
    electron.app.on('open-url', (event, url) => {
        logger.log(`Got URL from 'open-url' event: '${url}'.`)
        event.preventDefault()
        callback(new URL(url))
    })

    // Second, register the callback for the `second-instance` event. This is used on Windows.
    electron.app.on('second-instance', (event, argv) => {
        logger.log(`Got data from 'second-instance' event: '${argv}'.`)
        urlCallbackCompleted()
        // Check if additional data is an object that contains the URL.
        const url = argsDenoteUrlOpenAttempt(argv.slice(argv.length - 1))
        if (url) {
            logger.log(`Got URL from second instance: '${url.toString()}'.`)
            event.preventDefault()
            callback(url)
        }
    })
}

export function expectUrlCallback() {
    logger.log('Expecting URL callback.')
    electron.app.requestSingleInstanceLock();
}

export function urlCallbackCompleted() {
    logger.log('URL callback completed.')
    electron.app.releaseSingleInstanceLock();
}
