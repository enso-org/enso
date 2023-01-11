/// This module is responsible for loading the WASM binary, its dependencies, and providing the
/// user with a visual representation of this process (welcome screen). It also implements a view
/// allowing to choose a debug rendering test from.

// @ts-ignore
import globalConfig from '../../../../gui/config.yaml'
import buildCfg from '../../../build.json'
import { SemVer, Comparator } from 'semver'
import { App, logger, Param } from 'ensogl_app'
import { MixpanelLogger } from './mixpanel'

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
    static dev = new SemVer('0.0.0')
    static devPrerelease = 'dev'

    /// Version of the `client` js package.
    static ide = new SemVer(buildCfg.version, { loose: true })

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
        const comparator = new Comparator(`>=${minSupportedVersion}`)
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
    project: Param<string | null> = new Param(null, 'Project name to open on startup.')
    projectManager: Param<string | null> = new Param(
        null,
        'An address of the Project Manager service.'
    )
    languageServerRpc: Param<string | null> = new Param(
        null,
        'An address of the Language Server RPC endpoint. This argument should be provided ' +
            'together with `languageServerData` ,`namespace`, and `project` options. They make ' +
            'Enso connect directly to the already spawned Language Server of some project.'
    )
    languageServerData: Param<string | null> = new Param(
        null,
        'An address of the Language Server Data endpoint. This argument should be provided ' +
            'together with `languageServerData` ,`namespace`, and `project` options. They make ' +
            'Enso connect directly to the already spawned Language Server of some project.'
    )
    namespace: Param<string | null> = new Param(
        null,
        'Informs Enso about namespace of the opened project. May be used when connecting to ' +
            'existing Language Server process. Defaults to "local".'
    )
    platform: Param<string | null> = new Param(
        null,
        'The host platform the app is running on. This is used to adjust some UI elements. For ' +
            'example, on macOS, the window close buttons are integrated to the top app panel.'
    )
    frame: Param<boolean> = new Param(
        false,
        'Controls whether a window frame should be visible. Works in native app only.'
    )
    darkTheme: Param<boolean> = new Param(
        false,
        'Controls whether the dark theme should be used. Please note that the dark theme is not ' +
            'fully implemented yet.'
    )
    nodeLabels: Param<boolean> = new Param(true, `Controls whether node labels should be visible.`)
    dataGathering: Param<boolean> = new Param(
        true,
        'Controls whether anonymous data gathering should be enabled.'
    )
    isInCloud: Param<boolean> = new Param(false, 'Information if the app is running in the cloud.')
    authenticationEnabled: Param<boolean> = new Param(
        true,
        'Controls whether user authentication is enabled.'
    )
    email: Param<string | null> = new Param(null, 'The user email, if any.')
    applicationConfigUrl: Param<string> = new Param(
        'https://raw.githubusercontent.com/enso-org/ide/develop/config.json',
        'The application config URL. Used to check for available updates.'
    )
    testWorkflow: Param<string | null> = new Param(
        null,
        'When profiling the application (e.g. with the `./run profile` command), this argument ' +
            'chooses what is profiled.'
    )
    skipMinVersionCheck: Param<boolean> = new Param(
        Version.isDev(),
        'Controls whether the minimum engine version check should be performed. It is set to ' +
            '`true` in local builds.'
    )
    debug: Param<boolean> = new Param(
        Version.isDev(),
        'Controls whether the application should be run in the debug mode. In this mode all logs ' +
            'are printed to the console. Otherwise, the logs are hidden unless explicitly shown ' +
            'by calling `showLogs`. Moreover, additional logs from libraries are printed in ' +
            'this mode, including Mixpanel logs. The debug mode is set to `true` by default in ' +
            'local builds.'
    )
    preferredEngineVersion: Param<SemVer> = new Param(Version.ide, `The preferred engine version.`)
    enableNewComponentBrowser: Param<boolean> = new Param(
        true,
        'Controls whether the new component browser should be enabled.'
    )
}

// ========================
// === Main Entry Point ===
// ========================

class Main {
    async main(inputConfig: any) {
        const config = Object.assign(
            {
                mainWasmUrl: 'assets/main-opt.wasm',
                mainJsUrl: 'assets/main.js',
                shadersUrl: 'assets/shaders',
            },
            inputConfig
        )
        const app = new App({
            config,
            configExtension: new Config(),
            packageInfo: {
                version: BUILD_INFO.default.version,
                engineVersion: BUILD_INFO.default.engineVersion,
            },
        })

        if (app.initialized) {
            let mixpanelLogger = null
            if (app.config.dataGathering.value) {
                logger.log('Data gathering enabled. Initializing Mixpanel.')
                mixpanelLogger = new MixpanelLogger(app.config.debug.value)
                logger.addConsumer(mixpanelLogger)
            }
            if (!(await checkMinSupportedVersion(app.config))) {
                displayDeprecatedVersionDialog()
            } else {
                if (
                    app.config.authenticationEnabled.value &&
                    app.config.entry.value != app.config.entry.default
                ) {
                    // TODO: authentication here
                    // app.config.email.value = user.email
                    app.run()
                } else {
                    app.run()
                }
                if (app.config.email.value && mixpanelLogger) {
                    logger.log(`User identified as '${app.config.email.value}'.`)
                    mixpanelLogger.identify(app.config.email.value)
                }
            }
        }
    }
}

const API = new Main()

// @ts-ignore
window[globalConfig.windowAppScopeName] = API
