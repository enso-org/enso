/** @file This module is responsible for loading the WASM binary, its dependencies, and providing
 * the user with a visual representation of this process (welcome screen). It also implements a view
 * allowing to choose a debug rendering test from. */

import * as contentConfig from 'enso-content-config'
import * as semver from 'semver'

import * as app from '../../../../../target/ensogl-pack/linked-dist/index'
import globalConfig from '../../../../gui/config.yaml'

// === Constants ===
const LOGGER = app.log.logger

/** One second in milliseconds. */
const SECOND = 1000

// =============
// === Fetch ===
// =============

const timeout = (time: number) => {
    const controller = new AbortController()
    setTimeout(() => { controller.abort(); }, time * SECOND)
    return controller
}

/** A version of `fetch` which timeouts after the provided time. */
async function fetchTimeout(url: string, timeoutSeconds: number): Promise<unknown> {
    return fetch(url, { signal: timeout(timeoutSeconds).signal }).then(response => {
        const statusCodeOK = 200
        if (response.status === statusCodeOK) {
            return response.json()
        } else {
            throw new Error(`Failed to fetch '${url}'. Response status: ${response.status}.`)
        }
    })
}

/** Return `true` if the current application version is still supported and `false` otherwise.
 *
 * Function downloads the application config containing the minimum supported version from GitHub
 * and compares it with the version of the `client` js package. When the function is unable to
 * download the application config, or one of the compared versions does not match the semver
 * scheme, it returns `true`. */
async function checkMinSupportedVersion(config: typeof contentConfig.OPTIONS) {
    if (config.groups.engine.options.skipMinVersionCheck.value) {
        return true
    }
    try {
        const appConfig = await fetchTimeout(config.groups.engine.options.configUrl.value, 300)
        if (
            appConfig !== null &&
            typeof appConfig === 'object' &&
            'minimumSupportedVersion' in appConfig
        ) {
            const minSupportedVersion = appConfig.minimumSupportedVersion
            if (typeof minSupportedVersion === 'string') {
                const comparator = new semver.Comparator(`>=${minSupportedVersion}`)
                return comparator.test(contentConfig.Version.ide)
            } else {
                LOGGER.error('The minimum supported version is not a string.')
            }
        } else {
            LOGGER.error('The application config is not an object.')
        }
    } catch (e) {
        console.error('Minimum version check failed.', e)
        return true
    }
}

/** Display information that the current app version is deprecated. */
function displayDeprecatedVersionDialog() {
    const versionCheckText = document.createTextNode(
        'This version is no longer supported. Please download a new one.'
    )

    const root = document.getElementById('root')
    const versionCheckDiv = document.createElement('div')
    versionCheckDiv.id = 'version-check'
    versionCheckDiv.className = 'auth-info'
    versionCheckDiv.style.display = 'block'
    versionCheckDiv.appendChild(versionCheckText)
    if (root === null) {
        console.error('Cannot find the root DOM element.')
    } else {
        root.appendChild(versionCheckDiv)
    }
}

// ========================
// === Main Entry Point ===
// ========================

interface StringConfig {
    [key: string]: StringConfig | string
}

class Main {
    async main(inputConfig: StringConfig) {
        const config = Object.assign(
            {
                loader: {
                    wasmUrl: 'pkg-opt.wasm',
                    jsUrl: 'pkg.js',
                    assetsUrl: 'dynamic-assets',
                },
            },
            inputConfig
        )

        const appInstance = new app.App({
            config,
            configOptions: contentConfig.OPTIONS,
            packageInfo: {
                version: BUILD_INFO.default.version,
                engineVersion: BUILD_INFO.default.engineVersion,
            },
        })

        if (appInstance.initialized) {
            if (contentConfig.OPTIONS.options.dataCollection.value) {
                // TODO: Add remote-logging here.
            }
            if (!(await checkMinSupportedVersion(contentConfig.OPTIONS))) {
                displayDeprecatedVersionDialog()
            } else {
                if (
                    contentConfig.OPTIONS.options.authentication.value &&
                    contentConfig.OPTIONS.groups.startup.options.entry.value !==
                        contentConfig.OPTIONS.groups.startup.options.entry.default
                ) {
                    // TODO: authentication here
                    // appInstance.config.email.value = user.email
                    void appInstance.run()
                } else {
                    void appInstance.run()
                }
                const email = contentConfig.OPTIONS.groups.authentication.options.email.value
                if (email !== null) {
                    LOGGER.log(`User identified as '${email}'.`)
                }
            }
        } else {
            console.error('Failed to initialize the application.')
        }
    }
}

const API = new Main()

// @ts-expect-error `globalConfig.windowAppScopeName` is not known at typecheck time.
// eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
window[globalConfig.windowAppScopeName] = API
