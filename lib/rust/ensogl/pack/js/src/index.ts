import * as html_utils from 'dom/dom'
import * as fs from 'system/fs'
import host from 'system/host'
import { Loader } from 'wasm/loader'
import { Task } from 'log/task'
import { Logger } from 'log/logger'
import { Config } from 'config/config'
import { Args, parseArgs } from 'config/arg-parser'
import { logRouter } from 'log/router'
import { EntryPoint, wasmFunctions } from 'wasm/entry-point'

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
            for (const [key, value] of Object.entries(this)) {
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
        const task = Task.start(`Downloading files ${config.mainJsUrl} and ${config.mainWasmUrl}.`)
        const snippets_fetch = await fetch(config.mainJsUrl.value)
        const wasm_fetch = await fetch(config.mainWasmUrl.value)
        const loader = new Loader([snippets_fetch, wasm_fetch], config)
        loader.done.then(() => task.end())
        // FIXME:
        // @ts-ignore
        const download_size = loader.show_total_bytes()

        const snippets_code = await snippets_fetch.text()
        const wasm = await compile_and_run_wasm(snippets_code, wasm_fetch)
        await loader.initialized
        return { wasm, loader }
    } else {
        const snippets_code = await fs.readFile(config.mainJsUrl.value, 'utf8')
        const wasm_main = await fs.readFile(config.mainWasmUrl.value)
        const wasm = await compile_and_run_wasm(snippets_code, wasm_main)
        const loader = null
        return { wasm, loader }
    }
}

/** Compiles and runs the downloaded WASM file. */
async function compile_and_run_wasm(snippets_code: string, wasm: Buffer | Response): Promise<any> {
    return await Task.asyncWith('Running wasm.', async () => {
        const snippets_fn = Function(
            `const module = {}
             ${snippets_code}
             module.exports.init = pkg_default
             return module.exports`
        )()
        return await snippets_fn.init(wasm)
    })
}

// ===========
// === App ===
// ===========

type AppArgs = {
    config?: Object
    info?: Object
}

/** The main application class. */
export class App {
    args: Args
    info: Object
    config: Config
    wasm: any
    loader: Loader | null
    logger: Logger
    wasmFunctions: string[]
    beforeMainEntryPoints: Map<string, EntryPoint>
    mainEntryPoints: Map<string, EntryPoint>
    task: Task | null

    constructor() {
        this.logger = Logger
    }

    /** Runs the application. If it is run in the browser, it will initialize DOM elements, display
     * a loader, and list of entry points if the provided entry point is missing. If it is run in
     * node, it will run before main entry points and then the provided command. */
    async run(appArgs?: AppArgs): Promise<void> {
        this.info = appArgs?.info ?? {}
        const inputConfig = appArgs?.config ?? {}
        this.config = new Config({ overrides: [inputConfig, host.urlParams()] })

        if (host.browser) {
            await this.init()
            this.runEntryPoints()
        } else {
            this.args = parseArgs()
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
        const { wasm, loader } = await load_wasm(this.config)
        this.wasm = wasm
        this.loader = loader
        this.wasmFunctions = wasmFunctions(this.wasm)
        this.beforeMainEntryPoints = EntryPoint.beforeMainEntryPoints(this.wasmFunctions)
        this.mainEntryPoints = EntryPoint.mainEntryPoints(this.wasmFunctions)
        this.initBrowser()
        new PackageInfo(this.info).display()
    }

    runBeforeMainEntryPoints() {
        if (!this.beforeMainEntryPoints.size) {
            return
        }
        const count = this.beforeMainEntryPoints.size
        Task.with(`Running ${count} before main entry points.`, () => {
            for (const entryPoint of this.beforeMainEntryPoints.values()) {
                const [time, _] = Task.withTimed(`Running ${entryPoint.displayName()}.`, () => {
                    this.wasm[entryPoint.name()]()
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
        const entryPointName = this.config.entry.value
        const entryPoint = this.mainEntryPoints.get(entryPointName)
        if (entryPoint) {
            this.runBeforeMainEntryPoints()
            Logger.log(`Running the main entry point: ${entryPoint.displayName()}.`)
            this.wasm[entryPoint.name()]()
        } else {
            this.show_debug_screen(`Unknown entry point '${entryPointName}'. `)
        }
    }

    generateShadersCode() {
        Task.with('Getting shaders code from EnsoGL.', () => {
            // @ts-ignore
            const out = rustGenShadersFn()
            // console.log('got', out)
        })
    }

    initBrowser() {
        if (host.browser) {
            this.style_root()
            this.printScamWarning()
            this.disableContextMenu()
            if (!this.config.debug) {
                logRouter.hideLogs()
            }
        }
    }

    disableContextMenu() {
        document.body.addEventListener('contextmenu', e => {
            e.preventDefault()
        })
    }

    style_root() {
        const root = document.getElementById('root')
        if (root != null) {
            root.style.backgroundColor = 'rgb(249,250,251)'
        }
    }

    /// Displays a debug screen which allows the user to run one of predefined debug examples.
    show_debug_screen(unknownEntryPoint?: string) {
        Logger.log('show_debug_screen')
        const msg = unknownEntryPoint ? `Unknown entry point '${unknownEntryPoint}'. ` : ''
        const div = html_utils.new_top_level_div()
        const div2 = document.createElement('div')
        const content = document.createTextNode(msg + 'Available entry points:')
        const ul = document.createElement('ul')
        div.style.position = 'absolute'
        div.style.zIndex = '1'
        div2.appendChild(content)
        div.appendChild(div2)
        div2.appendChild(ul)

        for (const entry_point of this.mainEntryPoints.values()) {
            const li = document.createElement('li')
            const a = document.createElement('a')
            const linkText = document.createTextNode(entry_point.strippedName)
            ul.appendChild(li)
            a.appendChild(linkText)
            a.title = entry_point.strippedName
            a.href = '?entry=' + entry_point.strippedName
            li.appendChild(a)
        }
    }

    printScamWarning() {
        const headerCss = `
            color : white;
            background : crimson;
            display : block;
            border-radius : 8px;
            font-weight : bold;
            padding: 10px 20px 10px 20px;
        `
        const headerCss1 = headerCss + 'font-size : 46px;'
        const headerCss2 = headerCss + 'font-size : 20px;'
        const msgCSS = 'font-size:16px;'

        const msg1 =
            'This is a browser feature intended for developers. If someone told you to ' +
            'copy-paste something here, it is a scam and will give them access to your ' +
            'account and data.'
        const msg2 =
            'See https://github.com/enso-org/enso/blob/develop/docs/security/selfxss.md for more ' +
            'information.'
        console.log('%cStop!', headerCss1)
        console.log('%cYou may be victim of a scam!', headerCss2)
        console.log('%c' + msg1, msgCSS)
        console.log('%c' + msg2, msgCSS)
    }
}

// ==========================
// === App Initialization ===
// ==========================

// const app = new App()
// export default app
// // app.run()
//

let rustGenShadersFn: any = null
function registerGetShadersRustFn(fn: any) {
    Logger.log(`Registering 'getShadersFn'.`)
    rustGenShadersFn = fn
}
host.exportGlobal({ registerGetShadersRustFn })

// ==============
// === FIXMES ===
// ==============

// FIXME: leftover from old script
// API[globalConfig.windowAppScopeConfigName] = config
