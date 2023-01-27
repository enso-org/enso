/// This module is responsible for loading the WASM binary, its dependencies, and providing the
/// user with a visual representation of this process (welcome screen). It also implements a view
/// allowing to choose a debug rendering test from.

// @ts-ignore
import globalConfig from '../../../../gui/config.yaml'
import buildCfg from '../../../build.json'
import * as app from 'ensogl_app'
import * as semver from 'semver'

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

// ===============
// === Version ===
// ===============

class Version {
    /// Development version.
    static dev = new semver.SemVer('0.0.0')
    static devPrerelease = 'dev'

    /// Version of the `client` js package.
    static ide = new semver.SemVer(buildCfg.version, { loose: true })

    static isDev(): boolean {
        const clientVersion = Version.ide
        const releaseDev = clientVersion.compareMain(Version.dev) === 0
        const prereleaseDev = clientVersion.prerelease.toString().includes(Version.devPrerelease)
        return releaseDev || prereleaseDev
    }
}

/// Return `true` if the current application version is still supported
/// and `false` otherwise.
///
/// Function downloads the application config containing the minimum supported
/// version from GitHub and compares it with the version of the `client` js
/// package. When the function is unable to download the application config, or
/// one of the compared versions does not match the semver scheme, it returns
/// `true`.
async function checkMinSupportedVersion(config: Config) {
    if (config.skipMinVersionCheck.value === true) {
        return true
    }
    try {
        const appConfig: any = await fetchTimeout(config.applicationConfigUrl.value, 300)
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
    root.appendChild(versionCheckDiv)
}

// ==============
// === Config ===
// ==============

class Config {
    project: config.Param<string | null> = new config.Param(
        null,
        'Project name to open on startup.'
    )
    projectManager: config.Param<string | null> = new config.Param(
        null,
        'An address of the Project Manager service.'
    )
    languageServerRpc: config.Param<string | null> = new config.Param(
        null,
        'An address of the Language Server RPC endpoint. This argument should be provided ' +
            'together with `languageServerData` ,`namespace`, and `project` options. They make ' +
            'Enso connect directly to the already spawned Language Server of some project.'
    )
    languageServerData: config.Param<string | null> = new config.Param(
        null,
        'An address of the Language Server Data endpoint. This argument should be provided ' +
            'together with `languageServerData` ,`namespace`, and `project` options. They make ' +
            'Enso connect directly to the already spawned Language Server of some project.'
    )
    namespace: config.Param<string | null> = new config.Param(
        null,
        'Informs Enso about namespace of the opened project. May be used when connecting to ' +
            'existing Language Server process. Defaults to "local".'
    )
    platform: config.Param<string | null> = new config.Param(
        null,
        'The host platform the app is running on. This is used to adjust some UI elements. For ' +
            'example, on macOS, the window close buttons are integrated to the top app panel.'
    )
    frame: config.Param<boolean> = new config.Param(
        false,
        'Controls whether a window frame should be visible. Works in native app only.'
    )
    darkTheme: config.Param<boolean> = new config.Param(
        false,
        'Controls whether the dark theme should be used. Please note that the dark theme is not ' +
            'fully implemented yet.'
    )
    nodeLabels: config.Param<boolean> = new config.Param(
        true,
        `Controls whether node labels should be visible.`
    )
    dataGathering: config.Param<boolean> = new config.Param(
        true,
        'Controls whether anonymous data gathering should be enabled.'
    )
    isInCloud: config.Param<boolean> = new config.Param(
        false,
        'Information if the app is running in the cloud.'
    )
    authenticationEnabled: config.Param<boolean> = new config.Param(
        true,
        'Controls whether user authentication is enabled.'
    )
    email: config.Param<string | null> = new config.Param(null, 'The user email, if any.')
    applicationConfigUrl: config.Param<string> = new config.Param(
        'https://raw.githubusercontent.com/enso-org/ide/develop/config.json',
        'The application config URL. Used to check for available updates.'
    )
    testWorkflow: config.Param<string | null> = new config.Param(
        null,
        'When profiling the application (e.g. with the `./run profile` command), this argument ' +
            'chooses what is profiled.'
    )
    skipMinVersionCheck: config.Param<boolean> = new config.Param(
        Version.isDev(),
        'Controls whether the minimum engine version check should be performed. It is set to ' +
            '`true` in local builds.'
    )
    debug: config.Param<boolean> = new config.Param(
        Version.isDev(),
        'Controls whether the application should be run in the debug mode. In this mode all logs ' +
            'are printed to the console. Otherwise, the logs are hidden unless explicitly shown ' +
            'by calling `showLogs`. Moreover, additional logs from libraries are printed in ' +
            'this mode. The debug mode is set to `true` by default in local builds.'
    )
    preferredEngineVersion: config.Param<semver.SemVer> = new config.Param(
        Version.ide,
        `The preferred engine version.`
    )
    enableNewComponentBrowser: config.Param<boolean> = new config.Param(
        true,
        'Controls whether the new component browser should be enabled.'
    )
    emitUserTimingMeasurements: config.Param<boolean> = new config.Param(false, 'TODO')
}

// ========================
// === Main Entry Point ===
// ========================

class Main {
    async main(inputConfig: any) {
        const config = Object.assign(
            {
                pkgWasmUrl: 'assets/pkg-opt.wasm',
                pkgJsUrl: 'assets/pkg.js',
                shadersUrl: 'assets/shaders',
            },
            inputConfig
        )
        const appInstance = new app.App({
            config,
            configExtension: new Config(),
            packageInfo: {
                version: BUILD_INFO.default.version,
                engineVersion: BUILD_INFO.default.engineVersion,
            },
        })

        if (appInstance.initialized) {
            if (appInstance.config.params.dataGathering.value) {
                // TODO: Add remote-logging here.
            }
            if (!(await checkMinSupportedVersion(appInstance.config.params))) {
                displayDeprecatedVersionDialog()
            } else {
                if (
                    appInstance.config.params.authenticationEnabled.value &&
                    appInstance.config.params.entry.value != appInstance.config.params.entry.default
                ) {
                    // TODO: authentication here
                    // appInstance.config.email.value = user.email
                    appInstance.run()
                } else {
                    appInstance.run()
                }
                if (appInstance.config.params.email.value) {
                    logger.log(`User identified as '${appInstance.config.params.email.value}'.`)
                }
            }
        }
    }
}

const API = new Main()

// @ts-ignore
window[globalConfig.windowAppScopeName] = API
