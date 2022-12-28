/// This module is responsible for loading the WASM binary, its dependencies, and providing the
/// user with a visual representation of this process (welcome screen). It also implements a view
/// allowing to choose a debug rendering test from.

// @ts-ignore
import * as loader_module from './loader'
// @ts-ignore
import * as html_utils from './html_utils'
// @ts-ignore
import assert from 'assert'


const runsInBrowser = typeof window !== 'undefined'

import fetch from 'node-fetch'
import * as fs from 'fs'


// ==================
// === Global API ===
// ==================

class PublicApi {
    run: (inputConfig: any) => Promise<void>

    log(event: string, data?: any) {
    }
}

const API = new PublicApi()


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


    if(runsInBrowser) {
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
        let wasm = await html_utils.log_group_collapsed(download_info, async () => {
            let wasm_glue_js = await wasm_glue_fetch.text()
            let wasm_glue = Function('let module = {};' + wasm_glue_js + '; return module')()
            console.log('WASM dependencies loaded.')
            console.log('Starting online WASM compilation.')

            // @ts-ignore
            return await wasm_glue.init(wasm_fetch)
        })
        console.log('WASM Compiled.')

        await loader.initialized
    }

    let wasm_glue_js = fs.readFileSync(config.wasm_glue_url, 'utf8')
    let wasm_glue = Function('let module = {};' + wasm_glue_js + '; module.exports.init = pkg_default; return module.exports')()
    let wasm_main = fs.readFileSync(config.wasm_url)
    let wasm = await wasm_glue.init(wasm_main)
    console.log(wasm_glue)

    let loader = {}
    return { wasm, loader }
}

// ====================
// === Debug Screen ===
// ====================

/// The name of the main scene in the WASM binary.
let main_entry_point = 'ide'

/// Prefix name of each scene defined in the WASM binary.
let wasm_entry_point_pfx = 'entry_point_'

/// Prefix name of each scene defined in the WASM binary.
let wasm_before_main_pfx = 'before_main_'

function wasm_functions(wasm: any): string[] {
    return Object.getOwnPropertyNames(wasm)
}

function wasm_before_main_functions(wasm: any): string[] {
    let names = []
    for (let fn of wasm_functions(wasm)) {
        if (fn.startsWith(wasm_before_main_pfx)) {
            names.push(fn)
        }
    }
    names.sort();
    return names
}

function wasm_entry_points_stripped(wasm: any): string[] {
    let names = []
    for (let fn of wasm_functions(wasm)) {
        if (fn.startsWith(wasm_entry_point_pfx)) {
            let name = fn.replace(wasm_entry_point_pfx, '')
            names.push(name)
        }
    }
    return names
}

/// Displays a debug screen which allows the user to run one of predefined debug examples.
function show_debug_screen(wasm: any, msg: string) {
    API.log('show_debug_screen')
    let names = wasm_entry_points_stripped(wasm)

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
        'See https://github.com/enso-org/enso/blob/develop/docs/security/selfxss.md for more ' +
        'information.'
    console.log('%cStop!', headerCSS1)
    console.log('%cYou may be victim of a scam!', headerCSS2)
    console.log('%c' + msg1, msgCSS)
    console.log('%c' + msg2, msgCSS)
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
        API.log('error', args)
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
// window.showLogs = showLogs

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
    API.log('crash', message)
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
    public entry: string = undefined
    public frame: boolean = false
    public theme: string = undefined
    public use_loader: boolean = true
    public wasm_url: string = 'main.wasm'
    public wasm_glue_url: string = 'snippets.js'
    public debug: boolean = false

    updateFromObject(other: any) {
        if (!ok(other)) {
            return
        }
        for (let key of Object.keys(this)) {
            let self: any = this
            let otherVal = other[key]
            let selfVal = self[key]
            if (ok(otherVal)) {
                if (typeof selfVal === 'boolean') {
                    let val = tryAsBoolean(otherVal)
                    if (val === null) {
                        console.error(
                            `Invalid value for ${key}: ${otherVal}. Expected boolean. Reverting to the default value of `
                        )
                    } else {
                        self[key] = val
                    }
                } else {
                    self[key] = tryAsString(otherVal)
                }
            }
        }
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

function tryAsBoolean(value: any): boolean | null {
    value = parseBooleanOrLeaveAsIs(value)
    return typeof value == 'boolean' ? value : null
}

function tryAsString(value: any): string {
    return value.toString()
}

export function register_get_shaders_fn(fn: any) {
    console.log("!!!!!!!!!!!!!!!!!!!")
}

global.register_get_shaders_fn = register_get_shaders_fn

/// Main entry point. Loads WASM, initializes it, chooses the scene to run.
async function runEntryPoint(config: Config) {
    // @ts-ignore
    // API[globalConfig.windowAppScopeConfigName] = config

    // // Build data injected during the build process. See `webpack.config.js` for the source.
    // // @ts-ignore
    // const hash = GIT_HASH
    // API.log('git_hash', { hash })
    // // @ts-ignore
    // const buildInfo = BUILD_INFO
    // API.log('build_information', buildInfo)
    // // @ts-ignore
    // const status = GIT_STATUS
    // API.log('git_status', { status })

    //initCrashHandling()
    if(runsInBrowser) {
        style_root()
        printScamWarning()
        disableContextMenu()
        if (!config.debug) {
            hideLogs()
        }
    }


    let entryTarget = ok(config.entry) ? config.entry : main_entry_point
    config.use_loader = config.use_loader && entryTarget === main_entry_point

    if(runsInBrowser) {
        API.log('window_show_animation')
        await windowShowAnimation()
    }
    API.log('download_content')
    let { wasm, loader } = await download_content(config)
    API.log('wasm_loaded')
    if (entryTarget) {
        let fn_name = wasm_entry_point_pfx + entryTarget
        let fn = wasm[fn_name]
        if (fn) {
            let before_main_fns = wasm_before_main_functions(wasm);
            if(before_main_fns) {
                console.log(`Running ${before_main_fns.length} before main functions.`);
                const t_start = performance.now();
                for (let before_main_fn_name of before_main_fns) {
                    wasm[before_main_fn_name]()
                }
                const t_end = performance.now();
                let ms = Math.round((t_end - t_start)*100)/100;
                console.log(`Before main functions took ${ms} milliseconds to run.`);
                if (ms > 30) {
                    console.error(`Before main functions took ${ms} milliseconds to run. This is too long. Before main functions should be used for fast initialization only.`);
                }
            }
            console.log(`Running the chosen entry point.`);
            // Loader will be removed by IDE after its initialization.
            // All other code paths need to call `loader.destroy()`.
            fn()
        } else {
            // loader.destroy()
            // show_debug_screen(wasm, "Unknown entry point '" + entryTarget + "'. ")
            let before_main_fns = wasm_before_main_functions(wasm);
            if(before_main_fns) {
                console.log(`Running ${before_main_fns.length} before main functions.`);
                const t_start = performance.now();
                for (let before_main_fn_name of before_main_fns) {
                    wasm[before_main_fn_name]()
                }
                const t_end = performance.now();
                let ms = Math.round((t_end - t_start)*100)/100;
                console.log(`Before main functions took ${ms} milliseconds to run.`);
                if (ms > 30) {
                    console.error(`Before main functions took ${ms} milliseconds to run. This is too long. Before main functions should be used for fast initialization only.`);
                }
            }
            console.log(`Running the chosen entry point.`);
            // Loader will be removed by IDE after its initialization.
            // All other code paths need to call `loader.destroy()`.
            // fn()
        }
    } else {
        // loader.destroy()
        show_debug_screen(wasm, '')
    }
}

function urlParams(): any {
    if(runsInBrowser) {
        const urlParams = new URLSearchParams(window.location.search)
        return Object.fromEntries(urlParams.entries())
    } else {
        {}
    }
}

API.run = async function (inputConfig: any) {


    const config = new Config()
    config.updateFromObject(inputConfig)
    config.updateFromObject(urlParams())
    await runEntryPoint(config)
}

API.run({})
