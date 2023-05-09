/** @file This module is responsible for loading the WASM binary, its dependencies, and providing
 * the user with a visual representation of this process (welcome screen). It also implements a view
 * allowing to choose a debug rendering test from. */

import * as semver from 'semver'

import * as authentication from 'enso-authentication'
import * as contentConfig from 'enso-content-config'

import * as app from '../../../../../target/ensogl-pack/linked-dist/index'
import GLOBAL_CONFIG from '../../../../gui/config.yaml' assert { type: 'yaml' }

const logger = app.log.logger

// =================
// === Constants ===
// =================

/** Path to the SSE endpoint over which esbuild sends events. */
const ESBUILD_PATH = '/esbuild'
/** SSE event indicating a build has finished. */
const ESBUILD_EVENT_NAME = 'change'
/** One second in milliseconds. */
const SECOND = 1000
/** Time in seconds after which a `fetchTimeout` ends. */
const FETCH_TIMEOUT = 300

// ===================
// === Live reload ===
// ===================

if (IS_DEV_MODE) {
    new EventSource(ESBUILD_PATH).addEventListener(ESBUILD_EVENT_NAME, () => {
        location.reload()
    })
}

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
    let supported = false
    if (config.groups.engine.options.skipMinVersionCheck.value) {
        supported = true
    } else {
        try {
            const appConfig = await fetchTimeout(
                config.groups.engine.options.configUrl.value,
                FETCH_TIMEOUT
            )
            if (
                typeof appConfig === 'object' &&
                appConfig != null &&
                'minimumSupportedVersion' in appConfig
            ) {
                const minSupportedVersion = appConfig.minimumSupportedVersion
                if (typeof minSupportedVersion !== 'string') {
                    logger.error('The minimum supported version is not a string.')
                } else {
                    const comparator = new semver.Comparator(`>=${minSupportedVersion}`)
                    supported = comparator.test(contentConfig.VERSION.ide)
                }
            } else {
                logger.error('The application config is not an object.')
            }
        } catch (e) {
            console.error('Minimum version check failed.', e)
            supported = true
        }
    }
    return supported
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
    if (root == null) {
        console.error('Cannot find the root DOM element.')
    } else {
        root.appendChild(versionCheckDiv)
    }
}

// ========================
// === Main entry point ===
// ========================

interface StringConfig {
    [key: string]: StringConfig | string
}

class Main implements AppRunner {
    app: app.App | null = null

    stopApp() {
        this.app?.stop()
    }

    async runApp(inputConfig?: StringConfig) {
        this.stopApp()

        /** FIXME: https://github.com/enso-org/enso/issues/6475
         * Default values names are out of sync with values used in code.
         * Rather than setting fixed values here we need to fix default values in config. */
        const config = Object.assign(
            {
                loader: {
                    wasmUrl: 'pkg-opt.wasm',
                },
            },
            inputConfig
        )

        this.app = new app.App({
            config,
            configOptions: contentConfig.OPTIONS,
            packageInfo: {
                version: BUILD_INFO.version,
                engineVersion: BUILD_INFO.engineVersion,
            },
        })

        if (!this.app.initialized) {
            console.error('Failed to initialize the application.')
        } else {
            if (contentConfig.OPTIONS.options.dataCollection.value) {
                // TODO: Add remote-logging here.
            }
            if (!(await checkMinSupportedVersion(contentConfig.OPTIONS))) {
                displayDeprecatedVersionDialog()
            } else {
                const email = contentConfig.OPTIONS.groups.authentication.options.email.value
                // The default value is `""`, so a truthiness check is most appropriate here.
                if (email) {
                    logger.log(`User identified as '${email}'.`)
                }
                void this.app.run()
            }
        }
    }

    main(inputConfig?: StringConfig) {
        contentConfig.OPTIONS.loadAll([app.urlParams()])
        const isUsingAuthentication = contentConfig.OPTIONS.options.authentication.value
        const isUsingNewDashboard =
            contentConfig.OPTIONS.groups.featurePreview.options.newDashboard.value
        const isOpeningMainEntryPoint =
            contentConfig.OPTIONS.groups.startup.options.entry.value ===
            contentConfig.OPTIONS.groups.startup.options.entry.default
        if ((isUsingAuthentication || isUsingNewDashboard) && isOpeningMainEntryPoint) {
            const hideAuth = () => {
                const auth = document.getElementById('dashboard')
                const ide = document.getElementById('root')
                if (auth) {
                    auth.style.display = 'none'
                }
                if (ide) {
                    ide.hidden = false
                }
            }
            /** This package is an Electron desktop app (i.e., not in the Cloud), so
             * we're running on the desktop. */
            /** TODO [NP]: https://github.com/enso-org/cloud-v2/issues/345
             * `content` and `dashboard` packages **MUST BE MERGED INTO ONE**. The IDE
             * should only have one entry point. Right now, we have two. One for the cloud
             * and one for the desktop. */
            const currentPlatform = contentConfig.OPTIONS.groups.startup.options.platform.value
            let platform = authentication.Platform.desktop
            if (currentPlatform === 'web') {
                platform = authentication.Platform.cloud
            }
            /** FIXME [PB]: https://github.com/enso-org/cloud-v2/issues/366
             * React hooks rerender themselves multiple times. It is resulting in multiple
             * Enso main scene being initialized. As a temporary workaround we check whether
             * appInstance was already ran. Target solution should move running appInstance
             * where it will be called only once. */
            let appInstanceRan = false
            const onAuthenticated = () => {
                if (!contentConfig.OPTIONS.groups.featurePreview.options.newDashboard.value) {
                    hideAuth()
                    if (!appInstanceRan) {
                        appInstanceRan = true
                        void this.runApp(inputConfig)
                    }
                }
            }
            authentication.run({
                appRunner: this,
                logger,
                platform,
                showDashboard:
                    contentConfig.OPTIONS.groups.featurePreview.options.newDashboard.value,
                onAuthenticated,
            })
        } else {
            void this.runApp(inputConfig)
        }
    }
}

// @ts-expect-error `globalConfig.windowAppScopeName` is not known at typecheck time.
// eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
window[GLOBAL_CONFIG.windowAppScopeName] = new Main()
