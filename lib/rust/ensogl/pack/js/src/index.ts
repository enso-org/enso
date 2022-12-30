/// This module is responsible for loading the WASM binary, its dependencies, and providing the
/// user with a visual representation of this process (welcome screen). It also implements a view
/// allowing to choose a debug rendering test from.

import Loader from './loader'
import * as html_utils from './html_utils'

import host from './host'
import Task from './task'
import Logger from './logger'
import { Config, DEFAULT_ENTRY_POINT } from './config'
import { ArgParser, parseArgs } from './arg-parser'

// import * as fs from 'fs'
let fs = require('./fs')

// ===================
// === PackageInfo ===
// ===================

/** Package info. It contains the build git hash and git status, both injected during build time.
 * See `bundle.ts` to learn more. It can also contain additional info provided by the user of this
 * library. */
class PackageInfo {
    gitHash: string
    gitStatus: string

    /** Constructor.
     * @param info - Optional user provided info. */
    constructor(info?: any) {
        const infoObject = info || {}
        // @ts-ignore
        this.gitHash = GIT_HASH
        // @ts-ignore
        this.gitStatus = GIT_STATUS
        const self: any = this
        Object.assign(self, infoObject)
    }

    /** Display the current info in the console. */
    display() {
        Logger.with('Package info.', () => {
            for (let [key, value] of Object.entries(this)) {
                if (value) {
                    Logger.log(`${key}: ${value}`)
                }
            }
        })
    }
}

// ========================
// === Content Download ===
// ========================

/** Loads the WASM binary and its dependencies. If it's run in the browser, the files will be
 * downloaded from a server and a loading progress indicator will be shown. If it's run in node, the
 * files will be read from disk. After the files are fetched, the WASM module is compiled and
 * initialized. */
async function load_wasm(config: Config) {
    if (host.browser) {
        let task = Task.start(`Downloading files ${config.snippets_url} and ${config.wasm_url}.`)
        let snippets_fetch = await fetch(config.snippets_url.value)
        let wasm_fetch = await fetch(config.wasm_url.value)
        let loader = new Loader([snippets_fetch, wasm_fetch], config)
        loader.done.then(() => task.end())
        // FIXME:
        let download_size = loader.show_total_bytes()

        let snippets_code = await snippets_fetch.text()
        let wasm = await init_wasm(snippets_code, wasm_fetch)
        await loader.initialized
        return { wasm, loader }
    } else {
        let snippets_code = await fs.readFile(config.snippets_url.value, 'utf8')
        let wasm_main = await fs.readFile(config.wasm_url.value)
        let wasm = await init_wasm(snippets_code, wasm_main)
        let loader = null
        return { wasm, loader }
    }
}

async function init_wasm(snippets_code: string, wasm: Buffer | Response): Promise<any> {
    return await Task.asyncWith('Wasm compilation.', async () => {
        let snippets_fn = Function(
            `let module = {}
             ${snippets_code}
             module.exports.init = pkg_default 
             return module.exports`
        )()
        return await snippets_fn.init(wasm)
    })
}

// ==================
// === EntryPoint ===
// ==================

const MAIN_ENTRY_POINT_PREFIX = 'entry_point_'
const BEFORE_MAIN_ENTRY_POINT_PREFIX = 'before_main_entry_point_'
const NAME_REGEX = new RegExp(String.raw`(?<underscore>__)|(_(?<specialChar>[0-9]+)_)`, 'g')

class EntryPoint {
    prefix: string
    name: string
    private constructor(prefix: string, name: string) {
        this.prefix = prefix
        this.name = name
    }

    fullName(): string {
        return this.prefix + this.name
    }

    displayName(): string {
        return this.name.replace(NAME_REGEX, (...args) => {
            let groups = args.at(-1)
            if (groups.underscore) {
                return '_'
            } else {
                return String.fromCharCode(parseInt(groups.specialChar))
            }
        })
    }

    static tryAs(prefix: string, fullName: string): EntryPoint | null {
        if (fullName.startsWith(prefix)) {
            return new EntryPoint(prefix, fullName.substring(prefix.length))
        } else {
            return null
        }
    }

    static tryAsMainEntryPoint(fullName: string): EntryPoint | null {
        return EntryPoint.tryAs(MAIN_ENTRY_POINT_PREFIX, fullName)
    }

    static tryAsBeforeMainEntryPoint(fullName: string): EntryPoint | null {
        return EntryPoint.tryAs(BEFORE_MAIN_ENTRY_POINT_PREFIX, fullName)
    }

    static entryPoints(prefix: string, names: string[]): Map<string, EntryPoint> {
        return names
            .map(n => EntryPoint.tryAs(prefix, n))
            .filter((n): n is EntryPoint => n != null)
            .reduce((map, n) => {
                map.set(n.name, n)
                return map
            }, new Map())
    }

    static mainEntryPoints(names: string[]): Map<string, EntryPoint> {
        return EntryPoint.entryPoints(MAIN_ENTRY_POINT_PREFIX, names)
    }

    static beforeMainEntryPoints(names: string[]): Map<string, EntryPoint> {
        return EntryPoint.entryPoints(BEFORE_MAIN_ENTRY_POINT_PREFIX, names)
    }
}

function wasmFunctions(wasm: any): string[] {
    let names = Object.getOwnPropertyNames(wasm)
    names.sort()
    return names
}

// =========================
// === EntryPointChooser ===
// =========================

/// Displays a debug screen which allows the user to run one of predefined debug examples.
function show_debug_screen(wasm: any, msg: string) {
    Logger.log('show_debug_screen')
    msg ??= ''
    let entryPoints = EntryPoint.mainEntryPoints(wasmFunctions(wasm))
    let debug_screen_div = html_utils.new_top_level_div()
    let newDiv = document.createElement('div')
    let newContent = document.createTextNode(msg + 'Available entry points:')
    let ul = document.createElement('ul')
    debug_screen_div.style.position = 'absolute'
    debug_screen_div.style.zIndex = '1'
    newDiv.appendChild(newContent)
    debug_screen_div.appendChild(newDiv)
    newDiv.appendChild(ul)

    for (let entry_point of entryPoints.values()) {
        let li = document.createElement('li')
        let a = document.createElement('a')
        let linkText = document.createTextNode(entry_point.name)
        ul.appendChild(li)
        a.appendChild(linkText)
        a.title = entry_point.name
        a.href = '?entry=' + entry_point.name
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
        Logger.log('error', args)
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

// ========================
// === Main Entry Point ===
// ========================

function style_root() {
    let root = document.getElementById('root')
    if (root != null) {
        root.style.backgroundColor = 'rgb(249,250,251)'
    }
}

function disableContextMenu() {
    document.body.addEventListener('contextmenu', e => {
        e.preventDefault()
    })
}

// @ts-ignore
host.global.registerGetShadersRustFn = registerGetShadersRustFn

function initBrowser(config: Config) {
    style_root()
    printScamWarning()
    disableContextMenu()
    if (!config.debug) {
        hideLogs()
    }
}

// ===========
// === App ===
// ===========

type AppArgs = {
    config?: Object
    info?: Object
}

export class App {
    args: ArgParser
    info: Object
    config: Config
    wasm: any
    loader: Loader | null
    logger: Logger
    wasmFunctions: string[]
    beforeMainEntryPoints: Map<string, EntryPoint>
    mainEntryPoints: Map<string, EntryPoint>
    task: Task | null
    getShadersFn: any

    constructor() {
        this.logger = Logger
    }

    async run(appArgs?: AppArgs): Promise<void> {
        this.args = parseArgs()
        this.info = appArgs?.info ?? {}
        const inputConfig = appArgs?.config ?? {}
        this.config = new Config(inputConfig, host.urlParams())

        if (host.browser) {
            await this.init()
            this.runEntryPoints()
        } else {
            if (this.args.genShadersCode.value) {
                await this.init()
                this.runBeforeMainEntryPoints()
                this.generateShadersCode()
            } else {
                this.args.printHelpAndExit()
            }
        }
        if (this.task) this.task.end()
    }

    async init() {
        if (host.node) this.task = Task.start('Running the program.')
        let { wasm, loader } = await load_wasm(this.config)
        this.wasm = wasm
        this.loader = loader
        this.wasmFunctions = wasmFunctions(this.wasm)
        this.beforeMainEntryPoints = EntryPoint.beforeMainEntryPoints(this.wasmFunctions)
        this.mainEntryPoints = EntryPoint.mainEntryPoints(this.wasmFunctions)
        this.initBrowser()
        new PackageInfo(this.info).display()
    }

    initBrowser() {
        if (host.browser) {
            initBrowser(this.config)
        }
    }

    runBeforeMainEntryPoints() {
        if (!this.beforeMainEntryPoints.size) {
            return
        }
        let count = this.beforeMainEntryPoints.size
        Task.with(`Running ${count} before main entry points.`, () => {
            for (let entryPoint of this.beforeMainEntryPoints.values()) {
                const [time, _] = Task.withTimed(`Running ${entryPoint.displayName()}.`, () => {
                    this.wasm[entryPoint.fullName()]()
                })
                this.checkBeforeMainEntryPointTime(time)
            }
        })
    }

    checkBeforeMainEntryPointTime(time: number) {
        if (time > this.config.maxBeforeMainEntryPointsTimeMs) {
            Logger.error(`Entry point took ${time} milliseconds to run. This is too long.`)
            Logger.error('Before main entry points should be used for fast initialization only.')
        }
    }

    runEntryPoints() {
        let entryPointName = this.config.entry.value
        let entryPoint = this.mainEntryPoints.get(entryPointName)
        if (entryPoint) {
            this.runBeforeMainEntryPoints()
            Logger.log(`Running the main entry point: ${entryPoint.displayName()}.`)
            this.wasm[entryPoint.fullName()]()
        } else {
            show_debug_screen(this.wasm, `Unknown entry point '${entryPointName}'. `)
        }
    }

    generateShadersCode() {
        Task.with('Generating shaders code.', () => {
            let out = this.getShadersFn()
            console.log('got', out)
        })
    }
}

// ==========================
// === App Initialization ===
// ==========================

console.log(window)
const app = new App()
export default app
app.run()

function registerGetShadersRustFn(fn: any) {
    Logger.log(`Registering 'getShadersFn'.`)
    app.getShadersFn = fn
}

// FIXME: leftover from old script
// API[globalConfig.windowAppScopeConfigName] = config
