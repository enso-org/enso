/// This module is responsible for loading the WASM binary, its dependencies, and providing the
/// user with a visual representation of this process (welcome screen). It also implements a view
/// allowing to choose a debug rendering test from.

// @ts-ignore
import * as loader_module from 'enso-studio-common/src/loader'
// @ts-ignore
import * as html_utils from 'enso-studio-common/src/html_utils'
// @ts-ignore
import * as globalConfig from '../../../../gui/config.yaml'
// @ts-ignore
import cfg from '../../../config'
// @ts-ignore
import assert from 'assert'

// @ts-ignore
import firebase from 'firebase/app'
// @ts-ignore
import 'firebase/auth'
// @ts-ignore
import semver from 'semver'

// @ts-ignore
import * as profiling from '../../profiling/src/profiling'

// ==================
// === Global API ===
// ==================

class ContentApi {
    main: (inputConfig: any) => Promise<void>
    private logger: MixpanelLogger

    initLogging(config: Config) {
        if (config.data_gathering) {
            this.logger = new MixpanelLogger()
            if (ok(config.email)) {
                this.logger.identify(config.email)
            }
        }
    }
    remoteLog(event: string, data?: any) {
        if (this.logger) {
            this.logger.log(event, data)
        }
    }
}

const API = new ContentApi()

// @ts-ignore
window[globalConfig.windowAppScopeName] = API

// ========================
// === Content Download ===
// ========================

let incorrect_mime_type_warning = `
'WebAssembly.instantiateStreaming' failed because your server does not serve wasm with
'application/wasm' MIME type. Falling back to 'WebAssembly.instantiate' which is slower.
`

async function wasm_instantiate_streaming(
    resource: Response,
    imports: WebAssembly.Imports
): Promise<WebAssembly.WebAssemblyInstantiatedSource> {
    try {
        return WebAssembly.instantiateStreaming(resource, imports)
    } catch (e) {
        if (resource.headers.get('Content-Type') !== 'application/wasm') {
            console.warn(`${incorrect_mime_type_warning} Original error:\n`, e)
            const buffer = await resource.arrayBuffer()
            return WebAssembly.instantiate(buffer, imports)
        } else {
            throw "Server not configured to serve WASM with 'application/wasm' mime type."
        }
    }
}

/// Downloads the WASM binary and its dependencies. Displays loading progress bar unless provided
/// with `{use_loader:false}` option.
async function download_content(config: { wasm_glue_url: RequestInfo; wasm_url: RequestInfo }) {
    let wasm_glue_fetch = await fetch(config.wasm_glue_url)
    let wasm_fetch = await fetch(config.wasm_url)
    let loader = new loader_module.Loader([wasm_glue_fetch, wasm_fetch], config)

    // TODO [mwu]
    // Progress indication for WASM loading is hereby capped at 30%.
    // The remaining 70% is meant for IDE initialization. Currently we have no means of tracking
    // it, so we keep spinner running at 30% to denote ongoing initialization.
    // See https://github.com/enso-org/ide/issues/1237 for an immediate reason.
    // See https://github.com/enso-org/ide/issues/1105 for a broader context.
    loader.cap_progress_at = 0.3

    loader.done.then(() => {
        console.groupEnd()
        console.log('Download finished. Finishing WASM compilation.')
    })

    let download_size = loader.show_total_bytes()
    let download_info = `Downloading WASM binary and its dependencies (${download_size}).`
    let wasm_loader = html_utils.log_group_collapsed(download_info, async () => {
        let wasm_glue_js = await wasm_glue_fetch.text()
        let wasm_glue = Function('let exports = {};' + wasm_glue_js + '; return exports')()
        let imports = wasm_glue.wasm_imports()
        console.log('WASM dependencies loaded.')
        console.log('Starting online WASM compilation.')
        let wasm_loader = await wasm_instantiate_streaming(wasm_fetch, imports)
        // @ts-ignore
        wasm_loader.wasm_glue = wasm_glue
        return wasm_loader
    })

    // @ts-ignore
    let wasm = await wasm_loader.then(({ instance, module, wasm_glue }) => {
        let wasm = instance.exports
        wasm_glue.after_load(wasm, module)
        return wasm
    })
    console.log('WASM Compiled.')

    await loader.initialized
    return { wasm, loader }
}

// ====================
// === Debug Screen ===
// ====================

/// The name of the main scene in the WASM binary.
let main_entry_point = 'ide'

/// Prefix name of each scene defined in the WASM binary.
let wasm_entry_point_pfx = 'entry_point_'

/// Displays a debug screen which allows the user to run one of predefined debug examples.
function show_debug_screen(wasm: any, msg: string) {
    API.remoteLog('show_debug_screen')
    let names = []
    for (let fn of Object.getOwnPropertyNames(wasm)) {
        if (fn.startsWith(wasm_entry_point_pfx)) {
            let name = fn.replace(wasm_entry_point_pfx, '')
            names.push(name)
        }
    }

    if (msg === '' || msg === null || msg === undefined) {
        msg = ''
    }
    let debug_screen_div = html_utils.new_top_level_div()
    let newDiv = document.createElement('div')
    let newContent = document.createTextNode(msg + 'Available entry points:')
    let ul = document.createElement('ul')
    debug_screen_div.style.position = 'absolute'
    debug_screen_div.style.zIndex = 1
    newDiv.appendChild(newContent)
    debug_screen_div.appendChild(newDiv)
    newDiv.appendChild(ul)

    for (let name of names) {
        let li = document.createElement('li')
        let a = document.createElement('a')
        let linkText = document.createTextNode(name)
        ul.appendChild(li)
        a.appendChild(linkText)
        a.title = name
        a.href = '?entry=' + name
        li.appendChild(a)
    }
}

// ====================
// === Scam Warning ===
// ====================

function printScamWarning() {
    let headerCSS = `
        color : white;
        background : crimson;
        display : block;
        border-radius : 8px;
        font-weight : bold;
        padding: 10px 20px 10px 20px;
    `
    let headerCSS1 = headerCSS + 'font-size : 46px;'
    let headerCSS2 = headerCSS + 'font-size : 20px;'
    let msgCSS = 'font-size:16px;'

    let msg1 =
        'This is a browser feature intended for developers. If someone told you to ' +
        'copy-paste something here, it is a scam and will give them access to your ' +
        'account and data.'
    let msg2 =
        'See https://github.com/enso-org/enso/tree/develop/gui/docs/security/selfxss.md for more ' +
        'information.'
    console.log('%cStop!', headerCSS1)
    console.log('%cYou may be victim of a scam!', headerCSS2)
    console.log('%c' + msg1, msgCSS)
    console.log('%c' + msg2, msgCSS)
}

// ======================
// === Remote Logging ===
// ======================

class MixpanelLogger {
    private readonly mixpanel: any

    constructor() {
        this.mixpanel = require('mixpanel-browser')
        this.mixpanel.init(
            '5b541aeab5e08f313cdc1d1bbebc12ac',
            { api_host: 'https://api-eu.mixpanel.com' },
            ''
        )
    }

    log(event: string, data: any) {
        if (this.mixpanel) {
            event = MixpanelLogger.trim_message(event)
            if (data !== undefined && data !== null) {
                data = MixpanelLogger.trim_message(JSON.stringify(data))
                this.mixpanel.track(event, { data })
            } else {
                this.mixpanel.track(event)
            }
        } else {
            console.warn(`Failed to log the event '${event}'.`)
        }
    }

    identify(uniqueId: string) {
        this.mixpanel.identify(uniqueId)
    }

    static trim_message(message: string) {
        const MAX_MESSAGE_LENGTH = 500
        let trimmed = message.substr(0, MAX_MESSAGE_LENGTH)
        if (trimmed.length < message.length) {
            trimmed += '...'
        }
        return trimmed
    }
}

// ======================
// === Logs Buffering ===
// ======================

const logsFns = ['log', 'info', 'debug', 'warn', 'error', 'group', 'groupCollapsed', 'groupEnd']

class LogRouter {
    private buffer: any[]
    private readonly raw: {}
    autoFlush: boolean

    constructor() {
        this.buffer = []
        this.raw = {}
        this.autoFlush = true
        // @ts-ignore
        console.autoFlush = true
        for (let name of logsFns) {
            // @ts-ignore
            this.raw[name] = console[name]
            // @ts-ignore
            console[name] = (...args) => {
                this.handle(name, args)
            }
        }
    }

    auto_flush_on() {
        this.autoFlush = true
        // @ts-ignore
        console.autoFlush = true
        for (let { name, args } of this.buffer) {
            // @ts-ignore
            this.raw[name](...args)
        }
        this.buffer = []
    }

    handle(name: string, args: any[]) {
        if (this.autoFlush) {
            // @ts-ignore
            this.raw[name](...args)
        } else {
            this.buffer.push({ name, args })
        }

        // The following code is just a hack to discover if the logs start with `[E]` which
        // indicates errors from Rust logger.
        if (name == 'error') {
            this.handleError(...args)
        } else if (name == 'log') {
            let firstArg = args[0]
            if (firstArg !== undefined) {
                if (!(typeof firstArg === 'string' || firstArg instanceof String)) {
                    firstArg = firstArg.toString()
                }
                if (firstArg.startsWith('%c')) {
                    let firstArgBody = firstArg.slice(2)
                    let bodyStartIndex = firstArgBody.indexOf('%c')
                    if (bodyStartIndex !== -1) {
                        let body = firstArgBody.slice(bodyStartIndex + 3)
                        let is_error = body.startsWith('[E]')
                        if (is_error) {
                            this.handleError(body)
                        }
                    }
                }
            }
        }
    }

    handleError(...args: any[]) {
        API.remoteLog('error', args)
    }
}

let logRouter = new LogRouter()

function hideLogs() {
    console.log('All subsequent logs will be hidden. Eval `showLogs()` to reveal them.')
    logRouter.autoFlush = false
    // @ts-ignore
    console.autoFlush = false
}

function showLogs() {
    logRouter.auto_flush_on()
}

// @ts-ignore
window.showLogs = showLogs

function showPerformanceReport() {
    const report = new profiling.Report()
    report.renderToDevConsole()
}

// @ts-ignore
window.showPerformanceReport = showPerformanceReport

// ======================
// === Crash Handling ===
// ======================

function initCrashHandling() {
    setupCrashDetection()
    if (previousCrashMessageExists()) {
        showCrashBanner(getPreviousCrashMessage())
        clearPreviousCrashMessage()
    }
}

const crashMessageStorageKey = 'crash-message'

function previousCrashMessageExists() {
    return sessionStorage.getItem(crashMessageStorageKey) !== null
}

function getPreviousCrashMessage() {
    return sessionStorage.getItem(crashMessageStorageKey)
}

function storeLastCrashMessage(message: string) {
    sessionStorage.setItem(crashMessageStorageKey, message)
}

function clearPreviousCrashMessage() {
    sessionStorage.removeItem(crashMessageStorageKey)
}

// === Crash detection ===

function setupCrashDetection() {
    // This will only have an effect if the GUI is running in V8.
    // (https://v8.dev/docs/stack-trace-api#compatibility)
    Error.stackTraceLimit = 100

    window.addEventListener('error', function (event) {
        // We prefer stack traces over plain error messages but not all browsers produce traces.
        if (ok(event.error) && ok(event.error.stack)) {
            handleCrash(event.error.stack)
        } else {
            handleCrash(event.message)
        }
    })
    window.addEventListener('unhandledrejection', function (event) {
        // As above, we prefer stack traces.
        // But here, `event.reason` is not even guaranteed to be an `Error`.
        handleCrash(event.reason.stack || event.reason.message || 'Unhandled rejection')
    })
}

function handleCrash(message: string) {
    API.remoteLog('crash', message)
    if (document.getElementById(crashBannerId) === null) {
        storeLastCrashMessage(message)
        location.reload()
    } else {
        // @ts-ignore
        for (let element of [...document.body.childNodes]) {
            // @ts-ignore
            if (element.id !== crashBannerId) {
                element.remove()
            }
        }
        document.getElementById(crashBannerContentId).insertAdjacentHTML(
            'beforeend',
            `<hr>
             <div>A second error occurred. This time, the IDE will not automatically restart.</div>`
        )
    }
}

// === Crash recovery ===

// Those IDs should be the same that are used in index.html.
const crashBannerId = 'crash-banner'
const crashBannerContentId = 'crash-banner-content'
const crashReportButtonId = 'crash-report-button'
const crashBannerCloseButtonId = 'crash-banner-close-button'

function showCrashBanner(message: string) {
    document.body.insertAdjacentHTML(
        'afterbegin',
        `<div id="${crashBannerId}">
            <button id="${crashBannerCloseButtonId}" class="icon-button">✖</button>
            <div id="${crashBannerContentId}">
                <button id="${crashReportButtonId}">Report</button>
                An internal error occurred and the Enso IDE has been restarted.
            </div>
        </div>`
    )

    const banner = document.getElementById(crashBannerId)
    const content = document.getElementById(crashBannerContentId)
    const report_button = document.getElementById(crashReportButtonId)
    const close_button = document.getElementById(crashBannerCloseButtonId)

    report_button.onclick = async _event => {
        try {
            await reportCrash(message)
            content.textContent = 'Thank you, the crash was reported.'
        } catch (e) {
            content.textContent = 'The crash could not be reported.'
        }
    }
    close_button.onclick = () => {
        banner.remove()
    }
}

async function reportCrash(message: string) {
    // @ts-ignore
    const crashReportHost = API[globalConfig.windowAppScopeConfigName].crash_report_host
    await fetch(`http://${crashReportHost}/`, {
        method: 'POST',
        mode: 'no-cors',
        headers: {
            'Content-Type': 'text/plain',
        },
        body: message,
    })
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

/// Utility methods helping to work with the versions.
class Versions {
    /// Development version.
    static devVersion = semver.coerce('0.0.0')
    /// Version of the `client` js package.
    static clientVersion = require('../../client/package.json').version

    /// Compare the application version with the minimum supported version and
    /// return `true` if the application version is supported, i.e. greater or
    /// equal than the minimum supported version.
    static compare(minSupportedVersion: string, applicationVersion: string): boolean {
        const appVersion = semver.clean(applicationVersion, { loose: true })
        if (ok(appVersion)) {
            if (semver.eq(appVersion, Versions.devVersion)) {
                return true
            } else {
                const minVersion = semver.clean(minSupportedVersion, { loose: true })
                if (ok(minVersion)) {
                    return semver.gte(appVersion, minVersion)
                } else {
                    throw new ErrorDetails('Failed to parse app version.', { applicationVersion })
                }
            }
        } else {
            throw new ErrorDetails('Failed to parse minimum supported version.', {
                minSupportedVersion,
            })
        }
    }

    static isDevVersion(): boolean {
        const clientVersion = Versions.clientVersion
        const appVersion = semver.clean(clientVersion, { loose: true })
        if (ok(appVersion)) {
            return semver.eq(appVersion, Versions.devVersion)
        } else {
            console.error('Failed to parse applicationVersion.', { clientVersion })
            return false
        }
    }
}

/// Fetch the application config from the provided url.
async function fetchApplicationConfig(url: string) {
    const https = require('https')
    const statusCodeOK = 200

    return new Promise((resolve: any, reject: any) => {
        https.get(url, (res: any) => {
            const statusCode = res.statusCode
            if (statusCode !== statusCodeOK) {
                reject(new ErrorDetails('Request failed.', { url, statusCode }))
            }

            res.setEncoding('utf8')
            let rawData = ''

            res.on('data', (chunk: any) => (rawData += chunk))

            res.on('end', () => {
                try {
                    resolve(JSON.parse(rawData))
                } catch (e) {
                    reject(e)
                }
            })

            res.on('error', (e: any) => reject(e))
        })
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
async function checkMinSupportedVersion(config: Config) {
    try {
        const appConfig: any = await fetchApplicationConfig(config.application_config_url)
        const clientVersion = Versions.clientVersion
        const minSupportedVersion = appConfig.minimumSupportedVersion

        console.log(`Application version check.`, { minSupportedVersion, clientVersion })
        return Versions.compare(minSupportedVersion, clientVersion)
    } catch (e) {
        console.error('Minimum version check failed.', e)
        return true
    }
}

// ======================
// === Authentication ===
// ======================

class FirebaseAuthentication {
    protected readonly config: any
    public readonly firebaseui: any
    public readonly ui: any

    public authCallback: any

    constructor(authCallback: any) {
        this.firebaseui = require('firebaseui')
        this.config = this.getFirebaseConfig()
        // initialize Firebase
        firebase.initializeApp(this.config)
        // create HTML markup
        this.createHtml()
        // initialize Firebase UI
        this.ui = new this.firebaseui.auth.AuthUI(firebase.auth())
        this.ui.disableAutoSignIn()
        this.authCallback = authCallback
        firebase.auth().onAuthStateChanged((user: any) => {
            if (ok(user)) {
                if (this.hasEmailAuth(user) && !user.emailVerified) {
                    document.getElementById('user-email-not-verified').style.display = 'block'
                    this.handleSignedOutUser()
                } else {
                    this.handleSignedInUser(user)
                }
            } else {
                this.handleSignedOutUser()
            }
        })
    }

    protected getFirebaseConfig() {
        const config = require('../firebase.yaml')
        // @ts-ignore
        const firebaseApiKey = FIREBASE_API_KEY
        if (ok(firebaseApiKey) && firebaseApiKey.length > 0) {
            // @ts-ignore
            config['apiKey'] = firebaseApiKey
            return config
        } else {
            throw new Error('Empty FIREBASE_API_KEY')
        }
    }

    protected hasEmailAuth(user: any): boolean {
        const emailProviderId = firebase.auth.EmailAuthProvider.PROVIDER_ID
        const hasEmailProvider = user.providerData.some(
            (data: any) => data.providerId === emailProviderId
        )
        const hasOneProvider = user.providerData.length === 1
        return hasOneProvider && hasEmailProvider
    }

    protected getUiConfig() {
        return {
            callbacks: {
                // Called when the user has been successfully signed in.
                signInSuccessWithAuthResult: (authResult: any, redirectUrl: any) => {
                    if (ok(authResult.user)) {
                        switch (authResult.additionalUserInfo.providerId) {
                            case firebase.auth.EmailAuthProvider.PROVIDER_ID:
                                if (authResult.user.emailVerified) {
                                    this.handleSignedInUser(authResult.user)
                                } else {
                                    authResult.user.sendEmailVerification()
                                    document.getElementById(
                                        'user-email-not-verified'
                                    ).style.display = 'block'
                                    this.handleSignedOutUser()
                                }
                                break

                            default:
                        }
                    }
                    // Do not redirect.
                    return false
                },
            },
            signInOptions: [
                {
                    provider: firebase.auth.GoogleAuthProvider.PROVIDER_ID,
                    // Required to enable ID token credentials for this provider.
                    clientId: this.config.clientId,
                },
                firebase.auth.GithubAuthProvider.PROVIDER_ID,
                {
                    provider: firebase.auth.EmailAuthProvider.PROVIDER_ID,
                    // Whether the display name should be displayed in Sign Up page.
                    requireDisplayName: false,
                },
            ],
        }
    }

    protected handleSignedOutUser() {
        document.getElementById('auth-container').style.display = 'block'
        this.ui.start('#firebaseui-container', this.getUiConfig())
    }

    protected handleSignedInUser(user: any) {
        document.getElementById('auth-container').style.display = 'none'
        this.authCallback(user)
    }

    /// Create the HTML markup.
    ///
    /// ```
    /// <div id="root">
    ///     <div id="auth-container">
    ///         <div class="auth-header">
    ///             <h1>Sign in to Enso</h1>
    ///             <div class="auth-text">
    ///                 <p>Enso lets you create interactive data workflows. In order to share them, you need an account. In alpha/beta versions, this account is required.</p>
    ///             </div>
    ///         </div>
    ///         <div id="user-signed-out">
    ///             <div id="firebaseui-container"></div>
    ///         </div>
    ///         <div id="user-email-not-verified" class="auth-info">
    ///             Verification link is sent. You can sign in after verifying your email.
    ///         </div>
    ///     </div>
    ///     <div id="version-check" class="auth-info">
    ///         This version is no longer supported. Please download a new one.
    ///     </div>
    /// </div>
    /// ```
    protected createHtml() {
        const authContainer = 'auth-container'
        const authInfo = 'auth-info'
        const authHeader = 'auth-header'
        const authText = 'auth-text'
        const firebaseuiContainer = 'firebaseui-container'
        const userSignedOut = 'user-signed-out'
        const userEmailNotVerified = 'user-email-not-verified'
        const versionCheck = 'version-check'

        const authHeaderText = document.createTextNode('Sign in to Enso')
        const authTextText = document.createTextNode(
            'Enso lets you create interactive data workflows. In order to share them, you need an account. In alpha/beta versions, this account is required.'
        )
        const userEmailNotVerifiedText = document.createTextNode(
            'Verification link is sent. You can sign in after verifying your email.'
        )
        const versionCheckText = document.createTextNode(
            'This version is no longer supported. Please download a new one.'
        )

        let root = document.getElementById('root')

        // div#auth-container
        let authContainerDiv = document.createElement('div')
        authContainerDiv.id = authContainer
        authContainerDiv.style.display = 'none'
        // div.auth-header
        let authHeaderDiv = document.createElement('div')
        authHeaderDiv.className = authHeader
        // div.auth-header/h1
        let authHeaderH1 = document.createElement('h1')
        authHeaderH1.appendChild(authHeaderText)
        // div.auth-header/div#auth-text
        let authHeaderTextDiv = document.createElement('div')
        authHeaderTextDiv.className = authText
        authHeaderTextDiv.appendChild(authTextText)

        authHeaderDiv.appendChild(authHeaderH1)
        authHeaderDiv.appendChild(authHeaderTextDiv)

        // div#user-signed-out
        let userSignedOutDiv = document.createElement('div')
        userSignedOutDiv.id = userSignedOut
        let firebaseuiContainerDiv = document.createElement('div')
        firebaseuiContainerDiv.id = firebaseuiContainer
        userSignedOutDiv.appendChild(firebaseuiContainerDiv)

        // div#user-email-not-verified
        let userEmailNotVerifiedDiv = document.createElement('div')
        userEmailNotVerifiedDiv.id = userEmailNotVerified
        userEmailNotVerifiedDiv.className = authInfo
        userEmailNotVerifiedDiv.appendChild(userEmailNotVerifiedText)

        authContainerDiv.appendChild(authHeaderDiv)
        authContainerDiv.appendChild(userSignedOutDiv)
        authContainerDiv.appendChild(userEmailNotVerifiedDiv)

        // div#version-check
        let versionCheckDiv = document.createElement('div')
        versionCheckDiv.id = versionCheck
        versionCheckDiv.className = authInfo
        versionCheckDiv.appendChild(versionCheckText)

        root.appendChild(authContainerDiv)
        root.appendChild(versionCheckDiv)
    }
}

// ========================
// === Main Entry Point ===
// ========================

function style_root() {
    let root = document.getElementById('root')
    root.style.backgroundColor = 'rgb(249,250,251)'
}

/// Waits for the window to finish its show animation. It is used when the website is run in
/// Electron. Please note that it returns immediately in the web browser.
async function windowShowAnimation() {
    // @ts-ignore
    await window.showAnimation
}

function disableContextMenu() {
    document.body.addEventListener('contextmenu', e => {
        e.preventDefault()
    })
}

function ok(value: any) {
    return value !== null && value !== undefined
}

class Config {
    public entry: string
    public project: string
    public project_manager: string
    public language_server_rpc: string
    public language_server_data: string
    public namespace: string
    public platform: string
    public frame: boolean
    public theme: string
    public dark_theme: boolean
    public high_contrast: boolean
    public use_loader: boolean
    public wasm_url: string
    public wasm_glue_url: string
    public node_labels: boolean
    public crash_report_host: string
    public data_gathering: boolean
    public is_in_cloud: boolean
    public verbose: boolean
    public authentication_enabled: boolean
    public email: string
    public application_config_url: string

    static default() {
        let config = new Config()
        config.use_loader = true
        config.wasm_url = '/assets/ide.wasm'
        config.wasm_glue_url = '/assets/wasm_imports.js'
        config.crash_report_host = cfg.defaultLogServerHost
        config.data_gathering = true
        config.is_in_cloud = false
        config.entry = null
        config.authentication_enabled = true
        config.application_config_url =
            'https://raw.githubusercontent.com/enso-org/ide/develop/config.json'
        return config
    }

    updateFromObject(other: any) {
        if (!ok(other)) {
            return
        }
        this.entry = ok(other.entry) ? tryAsString(other.entry) : this.entry
        this.project = ok(other.project) ? tryAsString(other.project) : this.project
        this.project_manager = ok(other.project_manager)
            ? tryAsString(other.project_manager)
            : this.project_manager
        this.language_server_rpc = ok(other.language_server_rpc)
            ? tryAsString(other.language_server_rpc)
            : this.language_server_rpc
        this.language_server_data = ok(other.language_server_data)
            ? tryAsString(other.language_server_data)
            : this.language_server_data
        this.namespace = ok(other.namespace) ? tryAsString(other.namespace) : this.namespace
        this.platform = ok(other.platform) ? tryAsString(other.platform) : this.platform
        this.frame = ok(other.frame) ? tryAsBoolean(other.frame) : this.frame
        this.theme = ok(other.theme) ? tryAsString(other.theme) : this.theme
        this.dark_theme = ok(other.dark_theme) ? tryAsBoolean(other.dark_theme) : this.dark_theme
        this.high_contrast = ok(other.high_contrast)
            ? tryAsBoolean(other.high_contrast)
            : this.high_contrast
        this.use_loader = ok(other.use_loader) ? tryAsBoolean(other.use_loader) : this.use_loader
        this.wasm_url = ok(other.wasm_url) ? tryAsString(other.wasm_url) : this.wasm_url
        this.wasm_glue_url = ok(other.wasm_glue_url)
            ? tryAsString(other.wasm_glue_url)
            : this.wasm_glue_url
        this.node_labels = ok(other.node_labels)
            ? tryAsBoolean(other.node_labels)
            : this.node_labels
        this.crash_report_host = ok(other.crash_report_host)
            ? tryAsString(other.crash_report_host)
            : this.crash_report_host
        this.data_gathering = parseBoolean(other.data_gathering) ?? this.data_gathering
        this.is_in_cloud = ok(other.is_in_cloud)
            ? tryAsBoolean(other.is_in_cloud)
            : this.is_in_cloud
        this.verbose = ok(other.verbose) ? tryAsBoolean(other.verbose) : this.verbose
        this.authentication_enabled = ok(other.authentication_enabled)
            ? tryAsBoolean(other.authentication_enabled)
            : this.authentication_enabled
        this.email = ok(other.email) ? tryAsString(other.email) : this.email
        this.application_config_url = ok(other.application_config_url)
            ? tryAsString(other.application_config_url)
            : this.application_config_url
    }
}

function parseBoolean(value: any): boolean | null {
    if (value === 'true' || value === true) {
        return true
    } else if (value === 'false' || value === false) {
        return false
    } else {
        return null
    }
}

/// Check whether the value is a string with value `"true"`/`"false"`, if so, return the
// appropriate boolean instead. Otherwise, return the original value.
function parseBooleanOrLeaveAsIs(value: any): any {
    if (value === 'true') {
        return true
    }
    if (value === 'false') {
        return false
    }
    return value
}

function tryAsBoolean(value: any): boolean {
    value = parseBooleanOrLeaveAsIs(value)
    assert(typeof value == 'boolean')
    return value
}

function tryAsString(value: any): string {
    return value.toString()
}

/// Main entry point. Loads WASM, initializes it, chooses the scene to run.
async function runEntryPoint(config: Config) {
    // @ts-ignore
    API[globalConfig.windowAppScopeConfigName] = config

    API.initLogging(config)

    // Build data injected during the build process. See `webpack.config.js` for the source.
    // @ts-ignore
    const hash = GIT_HASH
    API.remoteLog('git_hash', { hash })
    // @ts-ignore
    const buildInfo = BUILD_INFO
    API.remoteLog('build_information', buildInfo)
    // @ts-ignore
    const status = GIT_STATUS
    API.remoteLog('git_status', { status })

    //initCrashHandling()
    style_root()
    printScamWarning()
    hideLogs()
    disableContextMenu()

    let entryTarget = ok(config.entry) ? config.entry : main_entry_point
    config.use_loader = config.use_loader && entryTarget === main_entry_point

    API.remoteLog('window_show_animation')
    await windowShowAnimation()
    API.remoteLog('download_content')
    let { wasm, loader } = await download_content(config)
    API.remoteLog('wasm_loaded')
    if (entryTarget) {
        let fn_name = wasm_entry_point_pfx + entryTarget
        let fn = wasm[fn_name]
        if (fn) {
            fn()
        } else {
            loader.destroy()
            show_debug_screen(wasm, "Unknown entry point '" + entryTarget + "'. ")
        }
    } else {
        show_debug_screen(wasm, '')
    }
}

API.main = async function (inputConfig: any) {
    const config = profiling.task.measure('load_config', () => {
        const urlParams = new URLSearchParams(window.location.search)
        // @ts-ignore
        const urlConfig = Object.fromEntries(urlParams.entries())
        const config = Config.default()
        config.updateFromObject(inputConfig)
        config.updateFromObject(urlConfig)
        return config
    })

    if (await checkMinSupportedVersion(config)) {
        if (config.authentication_enabled && !Versions.isDevVersion()) {
            new FirebaseAuthentication(function (user: any) {
                config.email = user.email
                runEntryPoint(config)
            })
        } else {
            await runEntryPoint(config)
        }
    } else {
        // Display a message asking to update the application.
        document.getElementById('auth-container').style.display = 'none'
        document.getElementById('version-check').style.display = 'block'
    }
}
