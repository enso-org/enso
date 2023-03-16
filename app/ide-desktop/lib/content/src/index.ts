/** @file This module is responsible for loading the WASM binary, its dependencies, and providing
 * the user with a visual representation of this process (welcome screen). It also implements a view
 * allowing to choose a debug rendering test from. */

import * as semver from 'semver'

import * as authentication from 'enso-authentication'
import * as contentConfig from 'enso-content-config'

import * as app from '../../../../../target/ensogl-pack/linked-dist/index'
import GLOBAL_CONFIG from '../../../../gui/config.yaml' assert { type: 'yaml' }

const logger = app.log.logger

// === Constants ===
/** One second in milliseconds. */
const SECOND = 1000
const FETCH_TIMEOUT = 300

// =============
// === Fetch ===
// =============

function timeout(time: number) {
    const controller = new AbortController()
    setTimeout(() => {
        controller.abort()
    }, time * SECOND)
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
        const appConfig = await fetchTimeout(
            config.groups.engine.options.configUrl.value,
            FETCH_TIMEOUT
        )
        if (
            typeof appConfig === 'object' &&
            // `typeof x === 'object'` narrows to `object | null`, not `object | undefined`
            // eslint-disable-next-line no-restricted-syntax
            appConfig !== null &&
            'minimumSupportedVersion' in appConfig
        ) {
            const minSupportedVersion = appConfig.minimumSupportedVersion
            if (typeof minSupportedVersion === 'string') {
                const comparator = new semver.Comparator(`>=${minSupportedVersion}`)
                return comparator.test(contentConfig.VERSION.ide)
            } else {
                logger.error('The minimum supported version is not a string.')
            }
        } else {
            logger.error('The application config is not an object.')
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

    const root = document.getElementById('root') ?? undefined
    const versionCheckDiv = document.createElement('div')
    versionCheckDiv.id = 'version-check'
    versionCheckDiv.className = 'auth-info'
    versionCheckDiv.style.display = 'block'
    versionCheckDiv.appendChild(versionCheckText)
    if (root === undefined) {
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
                version: BUILD_INFO.version,
                engineVersion: BUILD_INFO.engineVersion,
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
                    const hideAuth = () => {
                        const auth = document.getElementById('dashboard')
                        if (auth) auth.style.display = 'none'
                    }
                    /** This package is an Electron desktop app (i.e., not in the Cloud), so
                     * we're running on the desktop. */
                    /** TODO [NP]: https://github.com/enso-org/cloud-v2/issues/345
                     * `content` and `dashboard` packages **MUST BE MERGED INTO ONE**. The IDE
                     * should only have one entry point. Right now, we have two. One for the cloud
                     * and one for the desktop. Once these are merged, we can't hardcode the
                     * platform here, and need to detect it from the environment. */
                    const platform = authentication.Platform.desktop
                    const onAuthenticated = () => {
                        hideAuth()
                        void appInstance.run()
                    }
                    authentication.run(logger, platform, onAuthenticated)
                } else {
                    void appInstance.run()
                }
                const email = contentConfig.OPTIONS.groups.authentication.options.email.value
                // The default value is `""`, so a truthiness check is most appropriate here.
                if (email) {
                    logger.log(`User identified as '${email}'.`)
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
window[GLOBAL_CONFIG.windowAppScopeName] = API
