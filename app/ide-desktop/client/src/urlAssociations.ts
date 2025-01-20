/** @file URL associations for the IDE. */

import * as electron from 'electron'
import electronIsDev from 'electron-is-dev'

import * as common from 'enso-common'

import * as contentConfig from '@/contentConfig'

const logger = contentConfig.logger

// ============================
// === Protocol Association ===
// ============================

/**
 * Register the application as a handler for our [deep link scheme]{@link common.DEEP_LINK_SCHEME}.
 *
 * This method is no-op when used under the Electron dev mode, as it requires special handling to
 * set up the process.
 *
 * It is also no-op on macOS, as the OS handles the URL opening by passing the `open-url` event to
 * the application, thanks to the information baked in our application by `electron-builder`.
 */
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

/**
 * Check if the given list of application startup arguments denotes an attempt to open a URL.
 *
 * For example, this happens on Windows when the browser redirects user using our
 * [deep link scheme]{@link common.DEEP_LINK_SCHEME}. On macOS this is not used, as the OS
 * handles the URL opening by passing the `open-url` event to the application.
 * @param clientArgs - A list of arguments passed to the application, stripped from the initial
 * executable name and any electron dev mode arguments.
 * @returns The URL to open, or `null` if no file was specified.
 */
export function argsDenoteUrlOpenAttempt(clientArgs: readonly string[]): URL | null {
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

let initialUrl: URL | null = null

/**
 * Handle the case where IDE is invoked with a URL to open.
 *
 * This happens on Windows when the browser redirects user using the deep link scheme.
 * @param openedUrl - The URL to open.
 */
export function handleOpenUrl(openedUrl: URL) {
  logger.log(`Opening URL '${openedUrl.toString()}'.`)
  // We must wait for the application to be ready and then send the URL to the renderer process.
  initialUrl = openedUrl
}

/**
 * Register the callback that will be called when the application is requested to open a URL.
 *
 * This method serves to unify the url handling between macOS and Windows. On macOS, the OS
 * handles the URL opening by passing the `open-url` event to the application. On Windows, a
 * new instance of the application is started and the URL is passed as a command line argument.
 * @param callback - The callback to call when the application is requested to open a URL.
 */
export function registerUrlCallback(callback: (url: URL) => void) {
  if (initialUrl != null) {
    logger.log(`Got URL from command line: '${initialUrl.toString()}'.`)
    callback(initialUrl)
  }

  // First, register the callback for the `open-url` event. This is used on macOS.
  electron.app.on('open-url', (event, url) => {
    logger.log(`Got URL from 'open-url' event: '${url}'.`)
    event.preventDefault()
    callback(new URL(url))
  })

  // Second, register the callback for the `second-instance` event. This is used on Windows.
  electron.app.on('second-instance', (event, _argv, _workingDir, additionalData) => {
    // Check if additional data is an object that contains the URL.
    const url =
      (
        additionalData != null &&
        typeof additionalData === 'object' &&
        'urlToOpen' in additionalData &&
        additionalData.urlToOpen instanceof URL
      ) ?
        additionalData.urlToOpen
      : null
    if (url) {
      logger.log(`Got URL from second instance: '${url.toString()}'.`)
      event.preventDefault()
      callback(url)
    }
  })
}
