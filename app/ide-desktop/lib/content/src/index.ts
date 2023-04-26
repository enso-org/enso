/** @file This module is responsible for loading the WASM binary, its dependencies, and providing
 * the user with a visual representation of this process (welcome screen). It also implements a view
 * allowing to choose a debug rendering test from. */

import * as semver from 'semver'

import * as authentication from 'enso-authentication'
import * as contentConfig from 'enso-content-config'

import * as app from '../../../../../target/ensogl-pack/linked-dist/index'

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
/** The `id` attribute of the element that the IDE will be rendered into. */
const IDE_ELEMENT_ID = 'root'

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

// Hack to mutate `configOptions.OPTIONS`
let currentAppInstance: app.App | null = new app.App({
    config: {
        loader: {
            wasmUrl: 'pkg-opt.wasm',
            jsUrl: 'pkg.js',
            assetsUrl: 'dynamic-assets',
        },
    },
    configOptions: contentConfig.OPTIONS,
    packageInfo: {
        version: BUILD_INFO.version,
        engineVersion: BUILD_INFO.engineVersion,
    },
})

function tryStopProject() {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
    currentAppInstance?.wasm?.drop?.()
}

async function runProject(inputConfig?: StringConfig) {
    tryStopProject()
    const rootElement = document.getElementById(IDE_ELEMENT_ID)
    if (!rootElement) {
        logger.error(`The root element (the element with ID '${IDE_ELEMENT_ID}') was not found.`)
    } else {
        while (rootElement.firstChild) {
            rootElement.removeChild(rootElement.firstChild)
        }
    }

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

    currentAppInstance = new app.App({
        config,
        configOptions: contentConfig.OPTIONS,
        packageInfo: {
            version: BUILD_INFO.version,
            engineVersion: BUILD_INFO.engineVersion,
        },
    })
    console.log('bruh', currentAppInstance)

    if (!currentAppInstance.initialized) {
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
            void currentAppInstance.run()
        }
    }
}

if (
    (contentConfig.OPTIONS.options.authentication.value ||
        contentConfig.OPTIONS.groups.featurePreview.options.newDashboard.value) &&
    contentConfig.OPTIONS.groups.startup.options.entry.value ===
        contentConfig.OPTIONS.groups.startup.options.entry.default
) {
    window.tryStopProject = tryStopProject
    window.runProject = runProject
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
     * and one for the desktop. Once these are merged, we can't hardcode the
     * platform here, and need to detect it from the environment. */
    const platform = authentication.Platform.desktop
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
                void runProject()
            }
        }
    }
    authentication.run({
        logger,
        platform,
        showDashboard: contentConfig.OPTIONS.groups.featurePreview.options.newDashboard.value,
        onAuthenticated,
    })
} else {
    void runProject()
}
