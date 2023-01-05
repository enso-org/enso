/// This module is responsible for loading the WASM binary, its dependencies, and providing the
/// user with a visual representation of this process (welcome screen). It also implements a view
/// allowing to choose a debug rendering test from.

// @ts-ignore
import globalConfig from '../../../../gui/config.yaml'
// @ts-ignore
import buildCfg from '../../../build.json'

import * as semver from 'semver'
import { SemVer, Comparator } from 'semver'

import * as https from 'https'

const authInfo = 'auth-info'

import { App, LogLevel, Consumer, logger, Param } from 'ensogl_app'
import { Mixpanel } from 'mixpanel-browser'

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

class Versions {
    /// Development version.
    static devVersion = new SemVer('0.0.0')
    static devPrerelease = 'dev'

    /// Version of the `client` js package.
    static ideVersion = new SemVer(buildCfg.version, { loose: true })

    static isDevVersion(): boolean {
        const clientVersion = Versions.ideVersion
        const releaseDev = clientVersion.compareMain(Versions.devVersion) === 0
        const prereleaseDev = clientVersion.prerelease.toString().includes(Versions.devPrerelease)
        return releaseDev || prereleaseDev
    }
}

class Config {
    project: Param<string | null> = new Param(null, 'Project name to open on startup.')
    projectManager: Param<string | null> = new Param(null, 'TODO')
    languageServerRpc: Param<string | null> = new Param(null, 'TODO')
    languageServerData: Param<string | null> = new Param(null, 'TODO')
    namespace: Param<string | null> = new Param(null, 'TODO')
    platform: Param<string | null> = new Param(
        null,
        `The host platform the app is running on. This is used to adjust some UI elements. For \
        example, on macOS, the window close buttons are integrated to the top app panel.`
    )
    frame: Param<boolean> = new Param(
        false,
        `Controls whether a window frame should be visible. Works in native app only.`
    )
    darkTheme: Param<boolean> = new Param(
        false,
        `Controls whether the dark theme should be used. Please note that the dark theme is not \
        fully implemented yet.`
    )
    nodeLabels: Param<boolean> = new Param(true, `Controls whether node labels should be visible.`)
    dataGathering: Param<boolean> = new Param(
        true,
        `Controls whether anonymous data gathering should be enabled.`
    )
    isInCloud: Param<boolean> = new Param(false, `Information if the app is running in the cloud.`)
    authenticationEnabled: Param<boolean> = new Param(
        true,
        `Controls whether user authentication is enabled.`
    )
    email: Param<string | null> = new Param(null, `The user email, if any.`)
    applicationConfigUrl: Param<string> = new Param(
        'https://raw.githubusercontent.com/enso-org/ide/develop/config.json',
        'The application config URL. Used to check for available updates.'
    )
    testWorkflow: Param<string | null> = new Param(null, 'TODO')
    skipMinVersionCheck: Param<boolean> = new Param(
        false,
        `Controls whether the minimum engine version check should be performed. It is set to \
         \`true\` in local builds.`
    )
    debug: Param<boolean> = new Param(
        Versions.isDevVersion(),
        `Controls whether the application should be run in the debug mode. In this mode all logs \
        are printed to the console. Otherwise, the logs are hidden unless explicitly shown by \
        calling \`showLogs\`. Moreover, additional logs from libraries are printed in this mode, \
        including Mixpanel logs. The debug mode is set to \`true\` by default in local builds.`
    )
    preferredEngineVersion: Param<SemVer> = new Param(
        Versions.ideVersion,
        `The preferred engine version.`
    )
    enableNewComponentBrowser: Param<boolean> = new Param(
        true,
        `Controls whether the new component browser should be enabled.`
    )
}

// ======================
// === MixpanelLogger ===
// ======================

class MixpanelLogger extends Consumer {
    mixpanel: Mixpanel
    groups: (string | null)[] = []

    constructor(debug: boolean) {
        super()
        this.mixpanel = require('mixpanel-browser')
        this.mixpanel.init(
            '5b541aeab5e08f313cdc1d1bbebc12ac',
            { debug, api_host: 'https://api-eu.mixpanel.com' },
            ''
        )
    }

    message(level: LogLevel, ...args: any[]) {
        if (args.length > 0) {
            this.mixpanelLog(level, args[0].toString(), args.slice(1))
        }
    }

    group(...args: any[]) {
        if (args.length > 0) {
            let message = args[0].toString()
            this.mixpanelLog('log', `start: ${message}`, args.slice(1))
            this.groups.push(message)
        } else {
            this.groups.push(null)
        }
    }

    groupCollapsed(...args: any[]) {
        this.group(...args)
    }

    groupEnd(...args: any[]) {
        let message = this.groups.pop()
        if (message != null) {
            this.mixpanelLog('log', `end: ${message}`, args)
        }
    }

    mixpanelLog(level: LogLevel, message: string, data: any) {
        const trimmedMessage = this.trim_message(message)
        try {
            let payload: any = { level }
            if (data != null) {
                // FIXME: make data passing more intelligent here. If arg is object, just pass it to mixpanel with stringified vlaues - to be checked in mixpanel manual
                payload.data = this.trim_message(JSON.stringify(data))
            }
            this.mixpanel.track(trimmedMessage, payload, response => {
                // Mixpanel returns 1 on success. To learn more, see
                // https://mixpanel.com/help/reference/http#tracking-events
                if (typeof response == 'number') {
                    if (response == 0) {
                        console.error('Failed to log the event with Mixpanel.')
                    }
                } else {
                    if (response.status == 0) {
                        console.warn(`Failed to log the event with Mixpanel: '${response.error}'.`)
                    }
                }
            })
        } catch {
            console.warn(`Failed to log the event with Mixpanel: '${trimmedMessage}'.`)
        }
    }

    trim_message(message: string): string {
        const MAX_MESSAGE_LENGTH = 500
        let trimmed = message.substring(0, MAX_MESSAGE_LENGTH)
        if (trimmed.length < message.length) {
            trimmed += '...'
        }
        return trimmed
    }

    identify(uniqueId: string) {
        this.mixpanel.identify(uniqueId)
    }
}

// =====================
// === Version Check ===
// =====================

// An error with the payload.
class ErrorDetails {
    public readonly message: string
    public readonly payload: any

    constructor(message: string, payload: any) {
        this.message = message
        this.payload = payload
    }
}

//
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
        return comparator.test(Versions.ideVersion)
    } catch (e) {
        console.error('Minimum version check failed.', e)
        return true
    }
}

// ========================
// === Main Entry Point ===
// ========================

function createVersionCheckHtml() {
    const versionCheckText = document.createTextNode(
        'This version is no longer supported. Please download a new one.'
    )

    let root = document.getElementById('root')
    let versionCheckDiv = document.createElement('div')
    versionCheckDiv.id = 'version-check'
    versionCheckDiv.className = authInfo
    versionCheckDiv.style.display = 'block'
    versionCheckDiv.appendChild(versionCheckText)
    root.appendChild(versionCheckDiv)
}

class Main {
    async main(inputConfig: any) {
        const app = new App({
            config: new Config(),
            packageInfo: {
                version: BUILD_INFO.default.version,
                engineVersion: BUILD_INFO.default.engineVersion,
            },
        })
        app.init({
            config: {
                mainWasmUrl: 'assets/main-opt.wasm',
                mainJsUrl: 'assets/main.js',
            },
        })

        let mixpanelLogger = null
        if (app.config.dataGathering.value) {
            logger.log('Data gathering enabled. Initializing Mixpanel.')
            mixpanelLogger = new MixpanelLogger(app.config.debug.value)
            logger.addConsumer(mixpanelLogger)
        }
        if (!(await checkMinSupportedVersion(app.config))) {
            createVersionCheckHtml()
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

const API = new Main()

// @ts-ignore
window[globalConfig.windowAppScopeName] = API
