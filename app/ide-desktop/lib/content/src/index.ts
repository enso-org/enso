/// This module is responsible for loading the WASM binary, its dependencies, and providing the
/// user with a visual representation of this process (welcome screen). It also implements a view
/// allowing to choose a debug rendering test from.

// @ts-ignore
import * as loader_module from 'enso-studio-common/src/loader'
// @ts-ignore
import * as html_utils from 'enso-studio-common/src/html_utils'
// @ts-ignore
import globalConfig from '../../../../gui/config.yaml'
// @ts-ignore
import { defaultLogServerHost } from '../../../config'
// @ts-ignore
import assert from 'assert'
// @ts-ignore
import buildCfg from '../../../build.json'

// @ts-ignore
import firebase from 'firebase/compat/app'
// @ts-ignore
import 'firebase/auth'
// @ts-ignore
import firebase_config from '../firebase.yaml'

import * as semver from 'semver'
import { SemVer, Comparator } from 'semver'

import * as https from 'https'

const authInfo = 'auth-info'

import { App, LogLevel, Consumer, logger, Param } from 'ensogl_app'
import { Mixpanel } from 'mixpanel-browser'

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
        Versions.isDevVersion(),
        `Controls whether the minimum engine version check should be performed. It is set to \
         \`true\` in local builds.`
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

    constructor() {
        super()
        this.mixpanel = require('mixpanel-browser')
        this.mixpanel.init(
            '5b541aeab5e08f313cdc1d1bbebc12ac',
            { debug: true, api_host: 'https://api-eu.mixpanel.com' },
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

// // @ts-ignore
// window.showLogs = showLogs

// // =====================
// // === Version Check ===
// // =====================
//
// // An error with the payload.
// class ErrorDetails {
//     public readonly message: string
//     public readonly payload: any
//
//     constructor(message: string, payload: any) {
//         this.message = message
//         this.payload = payload
//     }
// }
//
/// Utility methods helping to work with the versions.

// /// Fetch the application config from the provided url.
// async function fetchApplicationConfig(url: string) {
//     const statusCodeOK = 200
//
//     return new Promise((resolve: any, reject: any) => {
//         https.get(url, res => {
//             const statusCode = res.statusCode
//             if (statusCode !== statusCodeOK) {
//                 reject(new ErrorDetails('Request failed.', { url, statusCode }))
//                 return
//             }
//
//             res.setEncoding('utf8')
//             let rawData = ''
//
//             res.on('data', (chunk: any) => (rawData += chunk))
//
//             res.on('end', () => {
//                 try {
//                     resolve(JSON.parse(rawData))
//                 } catch (e) {
//                     reject(e)
//                 }
//             })
//
//             res.on('error', (e: any) => reject(e))
//         })
//     })
// }
//
// /// Return `true` if the current application version is still supported
// /// and `false` otherwise.
// ///
// /// Function downloads the application config containing the minimum supported
// /// version from GitHub and compares it with the version of the `client` js
// /// package. When the function is unable to download the application config, or
// /// one of the compared versions does not match the semver scheme, it returns
// /// `true`.
// async function checkMinSupportedVersion(config: Config) {
//     if (config.skipMinVersionCheck === true) {
//         return true
//     }
//     try {
//         const appConfig: any = await fetchApplicationConfig(config.applicationConfigUrl)
//         const clientVersion = Versions.ideVersion
//         const minSupportedVersion = appConfig.minimumSupportedVersion
//         const comparator = new Comparator(`>=${minSupportedVersion}`)
//         return comparator.test(Versions.ideVersion)
//     } catch (e) {
//         console.error('Minimum version check failed.', e)
//         return true
//     }
// }
//
// // ======================
// // === Authentication ===
// // ======================
//
// class FirebaseAuthentication {
//     protected readonly config: any
//     public readonly firebaseui: any
//     public readonly ui: any
//
//     public authCallback: any
//
//     constructor(authCallback: any) {
//         this.firebaseui = require('firebaseui')
//         this.config = firebase_config
//         // initialize Firebase
//         firebase.initializeApp(this.config)
//         // create HTML markup
//         this.createHtml()
//         // initialize Firebase UI
//         this.ui = new this.firebaseui.auth.AuthUI(firebase.auth())
//         this.ui.disableAutoSignIn()
//         this.authCallback = authCallback
//         firebase.auth().onAuthStateChanged((user: any) => {
//             if (ok(user)) {
//                 if (this.hasEmailAuth(user) && !user.emailVerified) {
//                     document.getElementById('user-email-not-verified').style.display = 'block'
//                     this.handleSignedOutUser()
//                 } else {
//                     this.handleSignedInUser(user)
//                 }
//             } else {
//                 this.handleSignedOutUser()
//             }
//         })
//     }
//
//     protected hasEmailAuth(user: any): boolean {
//         const emailProviderId = firebase.auth.EmailAuthProvider.PROVIDER_ID
//         const hasEmailProvider = user.providerData.some(
//             (data: any) => data.providerId === emailProviderId
//         )
//         const hasOneProvider = user.providerData.length === 1
//         return hasOneProvider && hasEmailProvider
//     }
//
//     protected getUiConfig() {
//         return {
//             callbacks: {
//                 // Called when the user has been successfully signed in.
//                 signInSuccessWithAuthResult: (authResult: any, redirectUrl: any) => {
//                     if (ok(authResult.user)) {
//                         switch (authResult.additionalUserInfo.providerId) {
//                             case firebase.auth.EmailAuthProvider.PROVIDER_ID:
//                                 if (authResult.user.emailVerified) {
//                                     this.handleSignedInUser(authResult.user)
//                                 } else {
//                                     authResult.user.sendEmailVerification()
//                                     document.getElementById(
//                                         'user-email-not-verified'
//                                     ).style.display = 'block'
//                                     this.handleSignedOutUser()
//                                 }
//                                 break
//
//                             default:
//                         }
//                     }
//                     // Do not redirect.
//                     return false
//                 },
//             },
//             signInOptions: [
//                 {
//                     provider: firebase.auth.GoogleAuthProvider.PROVIDER_ID,
//                     // Required to enable ID token credentials for this provider.
//                     clientId: this.config.clientId,
//                 },
//                 firebase.auth.GithubAuthProvider.PROVIDER_ID,
//                 {
//                     provider: firebase.auth.EmailAuthProvider.PROVIDER_ID,
//                     // Whether the display name should be displayed in Sign Up page.
//                     requireDisplayName: false,
//                 },
//             ],
//         }
//     }
//
//     protected handleSignedOutUser() {
//         document.getElementById('auth-container').style.display = 'block'
//         this.ui.start('#firebaseui-container', this.getUiConfig())
//     }
//
//     protected handleSignedInUser(user: any) {
//         document.getElementById('auth-container').style.display = 'none'
//         this.authCallback(user)
//     }
//
//     /// Create the HTML markup.
//     ///
//     /// ```
//     /// <div id="root">
//     ///     <div id="auth-container">
//     ///         <div class="auth-header">
//     ///             <h1>Sign in to Enso</h1>
//     ///             <div class="auth-text">
//     ///                 <p>Enso lets you create interactive data workflows. In order to share them, you need an account. In alpha/beta versions, this account is required.</p>
//     ///             </div>
//     ///         </div>
//     ///         <div id="user-signed-out">
//     ///             <div id="firebaseui-container"></div>
//     ///         </div>
//     ///         <div id="user-email-not-verified" class="auth-info">
//     ///             Verification link is sent. You can sign in after verifying your email.
//     ///         </div>
//     ///     </div>
//     ///     <div id="version-check" class="auth-info">
//     ///         This version is no longer supported. Please download a new one.
//     ///     </div>
//     /// </div>
//     /// ```
//     protected createHtml() {
//         const authContainer = 'auth-container'
//         const authHeader = 'auth-header'
//         const authText = 'auth-text'
//         const firebaseuiContainer = 'firebaseui-container'
//         const userSignedOut = 'user-signed-out'
//         const userEmailNotVerified = 'user-email-not-verified'
//
//         const authHeaderText = document.createTextNode('Sign in to Enso')
//         const authTextText = document.createTextNode(
//             'Enso lets you create interactive data workflows. In order to share them, you need an account. In alpha/beta versions, this account is required.'
//         )
//         const userEmailNotVerifiedText = document.createTextNode(
//             'Verification link is sent. You can sign in after verifying your email.'
//         )
//
//         let root = document.getElementById('root')
//
//         // div#auth-container
//         let authContainerDiv = document.createElement('div')
//         authContainerDiv.id = authContainer
//         authContainerDiv.style.display = 'none'
//         // div.auth-header
//         let authHeaderDiv = document.createElement('div')
//         authHeaderDiv.className = authHeader
//         // div.auth-header/h1
//         let authHeaderH1 = document.createElement('h1')
//         authHeaderH1.appendChild(authHeaderText)
//         // div.auth-header/div#auth-text
//         let authHeaderTextDiv = document.createElement('div')
//         authHeaderTextDiv.className = authText
//         authHeaderTextDiv.appendChild(authTextText)
//
//         authHeaderDiv.appendChild(authHeaderH1)
//         authHeaderDiv.appendChild(authHeaderTextDiv)
//
//         // div#user-signed-out
//         let userSignedOutDiv = document.createElement('div')
//         userSignedOutDiv.id = userSignedOut
//         let firebaseuiContainerDiv = document.createElement('div')
//         firebaseuiContainerDiv.id = firebaseuiContainer
//         userSignedOutDiv.appendChild(firebaseuiContainerDiv)
//
//         // div#user-email-not-verified
//         let userEmailNotVerifiedDiv = document.createElement('div')
//         userEmailNotVerifiedDiv.id = userEmailNotVerified
//         userEmailNotVerifiedDiv.className = authInfo
//         userEmailNotVerifiedDiv.appendChild(userEmailNotVerifiedText)
//
//         authContainerDiv.appendChild(authHeaderDiv)
//         authContainerDiv.appendChild(userSignedOutDiv)
//         authContainerDiv.appendChild(userEmailNotVerifiedDiv)
//
//         root.appendChild(authContainerDiv)
//     }
// }
//
// // ========================
// // === Main Entry Point ===
// // ========================
//

// /// Main entry point. Loads WASM, initializes it, chooses the scene to run.
// async function runEntryPoint(config: Config) {
//     console.log('runEntryPoint', config)
//     // @ts-ignore
//     API[globalConfig.windowAppScopeConfigName] = config
//
//     API.initLogging(config)
//
//     // Build data injected during the build process. See `webpack.config.js` for the source.
//     // @ts-ignore
//     const hash = GIT_HASH
//     API.remoteLog('git_hash', { hash })
//     // @ts-ignore
//     const buildInfo = BUILD_INFO
//     API.remoteLog('build_information', buildInfo)
//     // @ts-ignore
//     const status = GIT_STATUS
//     API.remoteLog('git_status', { status })
//
//     //initCrashHandling()
//     style_root()
//     printScamWarning()
//     /// Only hide logs in production, but show them when running a development version.
//     if (!Versions.isDevVersion()) {
//         hideLogs()
//     }
//     disableContextMenu()
//
//     let entryTarget = ok(config.entry) ? config.entry : main_entry_point
//     config.use_loader = config.use_loader && entryTarget === main_entry_point
//
//     API.remoteLog('window_show_animation')
//     await windowShowAnimation()
//     API.remoteLog('download_content')
//     let { wasm, loader } = await download_content(config)
//     API.remoteLog('wasm_loaded')
//     if (entryTarget) {
//         let fn_name = wasm_entry_point_pfx + entryTarget
//         let fn = wasm[fn_name]
//         if (fn) {
//             let before_main_fns = wasm_before_main_functions(wasm)
//             if (before_main_fns) {
//                 console.log(`Running ${before_main_fns.length} before main functions.`)
//                 const t_start = performance.now()
//                 for (let before_main_fn_name of before_main_fns) {
//                     wasm[before_main_fn_name]()
//                 }
//                 const t_end = performance.now()
//                 let ms = Math.round((t_end - t_start) * 100) / 100
//                 console.log(`Before main functions took ${ms} milliseconds to run.`)
//                 if (ms > 30) {
//                     console.error(
//                         `Before main functions took ${ms} milliseconds to run. This is too long. Before main functions should be used for fast initialization only.`
//                     )
//                 }
//             }
//             console.log(`Running the chosen entry point.`)
//             // Loader will be removed by IDE after its initialization.
//             // All other code paths need to call `loader.destroy()`.
//             fn()
//         } else {
//             loader.destroy()
//             show_debug_screen(wasm, "Unknown entry point '" + entryTarget + "'. ")
//         }
//     } else {
//         loader.destroy()
//         show_debug_screen(wasm, '')
//     }
// }
//
// function createVersionCheckHtml() {
//     // div#version-check
//     const versionCheckText = document.createTextNode(
//         'This version is no longer supported. Please download a new one.'
//     )
//
//     let root = document.getElementById('root')
//     let versionCheckDiv = document.createElement('div')
//     versionCheckDiv.id = 'version-check'
//     versionCheckDiv.className = authInfo
//     versionCheckDiv.style.display = 'block'
//     versionCheckDiv.appendChild(versionCheckText)
//     root.appendChild(versionCheckDiv)
// }
//
// API.main = async function (inputConfig: any) {
//     console.log('hello from main2')
// const urlParams = new URLSearchParams(window.location.search)
// // @ts-ignore
// const urlConfig = Object.fromEntries(urlParams.entries())
//
// const config = new Config()
// config.updateFromObject(inputConfig)
// config.updateFromObject(urlConfig)
//
// if (await checkMinSupportedVersion(config)) {
//     if (config.authenticationEnabled && !config.entry) {
//         new FirebaseAuthentication(function (user: any) {
//             config.email = user.email
//             runEntryPoint(config)
//         })
//     } else {
//         await runEntryPoint(config)
//     }
// } else {
//     // Display a message asking to update the application.
//     createVersionCheckHtml()
// }
// }

class Main {
    async main(inputConfig: any) {
        const app = new App({
            config: new Config(),
            packageInfo: {
                version: BUILD_INFO.default.version,
                engineVersion: BUILD_INFO.default.engineVersion,
            },
        })
        app.configResolved.then((config: Config) => {
            if (config.dataGathering.value) {
                logger.log('Data gathering enabled. Initializing Mixpanel.')
                const mixpanelLogger = new MixpanelLogger()
                logger.addConsumer(mixpanelLogger)
                if (config.email.value) {
                    logger.log(`User identified as '${config.email.value}'.`)
                    mixpanelLogger.identify(config.email.value)
                }
            }
        })
        app.run({
            config: {
                mainWasmUrl: 'assets/main-opt.wasm',
                mainJsUrl: 'assets/main.js',
            },
        })
    }
}

const API = new Main()

// @ts-ignore
window[globalConfig.windowAppScopeName] = API
