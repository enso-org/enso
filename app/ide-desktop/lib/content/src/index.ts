/// This module is responsible for loading the WASM binary, its dependencies, and providing the
/// user with a visual representation of this process (welcome screen). It also implements a view
/// allowing to choose a debug rendering test from.

// @ts-ignore
import globalConfig from '../../../../gui/config.yaml'
import buildCfg from '../../../build.json'
// @ts-ignore
import * as app from '../../../../../target/ensogl-pack/dist/index'
import * as semver from 'semver'
import { Version, options } from './config'

const logger = app.log.logger
const config = app.config

// =============
// === Fetch ===
// =============

const Timeout = (time: number) => {
    let controller = new AbortController()
    setTimeout(() => controller.abort(), time * 1000)
    return controller
}

async function fetchTimeout(url: string, timeout: number): Promise<any> {
    return fetch(url, { signal: Timeout(10).signal }).then(response => {
        const statusCodeOK = 200
        if (response.status === statusCodeOK) {
            return response.json()
        } else {
            throw new Error(`Failed to fetch '${url}'. Response status: ${response.status}.`)
        }
    })
}

/// Return `true` if the current application version is still supported
/// and `false` otherwise.
///
/// Function downloads the application config containing the minimum supported
/// version from GitHub and compares it with the version of the `client` js
/// package. When the function is unable to download the application config, or
/// one of the compared versions does not match the semver scheme, it returns
/// `true`.
async function checkMinSupportedVersion(config: typeof options) {
    if (config.groups.engine.options.skipMinVersionCheck.value === true) {
        return true
    }
    try {
        const appConfig: any = await fetchTimeout(config.groups.engine.options.configUrl.value, 300)
        const minSupportedVersion = appConfig.minimumSupportedVersion
        const comparator = new semver.Comparator(`>=${minSupportedVersion}`)
        return comparator.test(Version.ide)
    } catch (e) {
        console.error('Minimum version check failed.', e)
        return true
    }
}

function displayDeprecatedVersionDialog() {
    const versionCheckText = document.createTextNode(
        'This version is no longer supported. Please download a new one.'
    )

    let root = document.getElementById('root')
    let versionCheckDiv = document.createElement('div')
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
// === Main Entry Point ===
// ========================

class Main {
    async main(inputConfig: any) {
        // FIXME: use inputConfig
        const config = {
            loader: {
                wasmUrl: 'assets/pkg-opt.wasm',
                jsUrl: 'assets/pkg.js',
                shadersUrl: 'assets/shaders',
            },
        }

        const appInstance = new app.App({
            config,
            configOptions: options,
            packageInfo: {
                // @ts-ignore
                version: BUILD_INFO.default.version,
                // @ts-ignore
                engineVersion: BUILD_INFO.default.engineVersion,
            },
        })

        if (appInstance.initialized) {
            if (options.options.dataCollection.value) {
                // TODO: Add remote-logging here.
            }
            if (!(await checkMinSupportedVersion(options))) {
                displayDeprecatedVersionDialog()
            } else {
                if (
                    options.options.authentication.value &&
                    options.groups.startup.options.entry.value !=
                        options.groups.startup.options.entry.default
                ) {
                    // TODO: authentication here
                    // appInstance.config.email.value = user.email
                    appInstance.run()
                } else {
                    appInstance.run()
                }
                const email = options.groups.authentication.options.email.value
                if (email != null) {
                    logger.log(`User identified as '${email}'.`)
                }
            }
        } else {
            console.error('Failed to initialize the application.')
        }
    }
}

const API = new Main()

// @ts-ignore
window[globalConfig.windowAppScopeName] = API
