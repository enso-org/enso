import * as html_utils from 'dom/dom'
import * as fs from 'system/fs'
import host from 'system/host'
import { Loader } from 'wasm/loader'
import { Task } from 'log/task'
import { logger, Logger } from 'log/logger'
import { Config, ExternalConfig } from 'config/config'
import { Args, parseArgs } from 'config/arg-parser'
import { logRouter } from 'log/router'
import { EntryPoint, wasmFunctions } from 'wasm/entry-point'

export { logger, Logger, Consumer } from 'log/logger'
export { Param } from 'config/config'
export type { LogLevel } from 'log/logger'

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
        logger.with('Package info.', () => {
            for (const [key, value] of Object.entries(this)) {
                if (value) {
                    logger.log(`${key}: ${value}`)
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
        const mainJsUrl = config.mainJsUrl.value
        const mainWasmUrl = config.mainWasmUrl.value
        const task = Task.start(`Downloading files '${mainJsUrl}' and '${mainWasmUrl}'.`)
        const snippets_fetch = await fetch(mainJsUrl)
        const wasm_fetch = await fetch(mainWasmUrl)
        const loader = new Loader([snippets_fetch, wasm_fetch], config)
        loader.done.then(() => task.end())
        // FIXME:
        // @ts-ignore
        const download_size = loader.show_total_bytes()

        const snippets_code = await snippets_fetch.text()
        const wasm = await compile_and_run_wasm(snippets_code, wasm_fetch)
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
}

/** The main application class. */
export class App {
    args: Args
    packageInfo: PackageInfo
    config: Config
    wasm: any
    loader: Loader | null
    logger: Logger
    wasmFunctions: string[]
    beforeMainEntryPoints: Map<string, EntryPoint>
    mainEntryPoints: Map<string, EntryPoint>
    task: Task | null
    private initialized = false

    constructor(opts?: { config?: ExternalConfig; packageInfo: any }) {
        this.packageInfo = new PackageInfo(opts?.packageInfo ?? {})
        this.config = new Config()
        if (opts?.config) {
            this.config.extend(opts.config)
        }

        this.logger = logger
    }

    init(appArgs?: AppArgs): boolean {
        this.initialized = true
        this.initBrowser()
        const inputConfig = appArgs?.config ?? {}
        const unrecognizedParams = this.config.resolve({
            overrides: [inputConfig, host.urlParams()],
        })
        logger.log(`Resolved config:`, this.config.strigifiedKeyValueMap())
        if (unrecognizedParams) {
            this.showConfigOptions(unrecognizedParams)
            return false
        } else {
            return true
        }
    }

    /** Runs the application. If it is run in the browser, it will initialize DOM elements, display
     * a loader, and list of entry points if the provided entry point is missing. If it is run in
     * node, it will run before main entry points and then the provided command. */
    async run(appArgs?: AppArgs): Promise<void> {
        if (!this.initialized) {
            if (!this.init(appArgs)) {
                return
            }
        }
        if (host.browser) {
            await this.loadWasm()
            this.runEntryPoints()
        } else {
            this.args = parseArgs()
            if (this.args.genShadersCode.value) {
                await this.loadWasm()
                this.runBeforeMainEntryPoints()
                this.generateShadersCode()
            } else {
                this.args.printHelpAndExit()
            }
        }
        if (this.task) this.task.end()
    }

    async loadWasm() {
        if (host.node) this.task = Task.start('Running the program.')
        const { wasm, loader } = await load_wasm(this.config)
        this.wasm = wasm
        this.loader = loader
        this.wasmFunctions = wasmFunctions(this.wasm)
        this.beforeMainEntryPoints = EntryPoint.beforeMainEntryPoints(this.wasmFunctions)
        this.mainEntryPoints = EntryPoint.mainEntryPoints(this.wasmFunctions)
        this.packageInfo.display()
    }

    runBeforeMainEntryPoints() {
        if (!this.beforeMainEntryPoints.size) {
            return
        }
        const count = this.beforeMainEntryPoints.size
        const [time, _] = Task.withTimed(`Running ${count} before main entry points.`, () => {
            for (const entryPoint of this.beforeMainEntryPoints.values()) {
                Task.withTimed(`Running ${entryPoint.displayName()}.`, () => {
                    this.wasm[entryPoint.name()]()
                })
            }
        })
        this.checkBeforeMainEntryPointsTime(time)
    }

    checkBeforeMainEntryPointsTime(time: number) {
        if (time > this.config.maxBeforeMainEntryPointsTimeMs.value) {
            logger.error(`Entry points took ${time} milliseconds to run. This is too long.`)
            logger.error('Before main entry points should be used for fast initialization only.')
        }
    }

    runEntryPoints() {
        const entryPointName = this.config.entry.value
        const entryPoint = this.mainEntryPoints.get(entryPointName)
        if (entryPoint) {
            this.runBeforeMainEntryPoints()
            if (this.loader) this.loader.destroy()
            logger.log(`Running the main entry point: ${entryPoint.displayName()}.`)
            this.wasm[entryPoint.name()]()
        } else {
            if (this.loader) this.loader.destroy()
            this.showEntryPointSelector(entryPointName)
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
            this.disableContextMenu()
            if (this.config.debug.value) {
                logger.log('Application is run in debug mode. Logs will not be hidden.')
            } else {
                this.printScamWarning()
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
    showEntryPointSelector(unknownEntryPoint?: string) {
        logger.log('Showing entry point selection help screen.')
        const msg = unknownEntryPoint ? `Unknown entry point '${unknownEntryPoint}'. ` : ''
        const title = msg + 'Available entry points:'
        const entries = Array.from(this.mainEntryPoints.values()).map(entryPoint => {
            // FIXME: Currently, this does not work. It should be fixed by wasm-bindgen or wasm-pack
            //     team. See: https://github.com/rustwasm/wasm-bindgen/issues/3224
            const docsFn = this.wasm[entryPoint.docsFnName()]
            let description = 'No description.'
            if (docsFn) {
                const rustDocs = docsFn()
                if (rustDocs) {
                    description = rustDocs
                }
            }
            const href = '?entry=' + entryPoint.strippedName
            return new HelpScreenEntry(entryPoint.strippedName, [description], href)
        })

        const headers = ['Name', 'Description']
        new HelpScreen().display({ title, headers, entries }, this.mainEntryPoints, this.wasm)
    }

    showConfigOptions(unknownConfigOptions?: string[]) {
        logger.log('Showing config options help screen.')
        const msg = unknownConfigOptions
            ? `Unknown config options: '${unknownConfigOptions}'. `
            : ''
        const title = msg + 'Available options:'
        const entries = Array.from(Object.entries(this.config)).map(([key, option]) => {
            return new HelpScreenEntry(key, [option.description, option.default])
        })
        const headers = ['Name', 'Description', 'Default']
        new HelpScreen().display({ title, headers, entries }, this.mainEntryPoints, this.wasm)
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

class HelpScreenEntry {
    name: string
    values: string[]
    href?: string

    constructor(name: string, values: string[], href?: string) {
        this.name = name
        this.values = values
        this.href = href
    }
}

class HelpScreen {
    /// Displays a debug screen which allows the user to run one of predefined debug examples.
    display(
        cfg: { title: string; headers: string[]; entries: HelpScreenEntry[] },
        mainEntryPoints: Map<string, EntryPoint>,
        wasm: any
    ) {
        const padding = '8px'
        const background_radius = '8px'
        const div = html_utils.new_top_level_div()
        div.style.fontFamily = `"SF Pro Text","SF Pro Icons","Helvetica Neue","Helvetica","Arial",sans-serif`
        div.style.fontSize = '14px'
        div.style.overflow = 'scroll'
        const div2 = document.createElement('div')
        div2.style.padding = '10px'
        div.appendChild(div2)

        const title = document.createElement('div')
        title.style.fontWeight = 'bold'
        title.style.padding = '8px'
        const content = document.createTextNode(cfg.title)
        const table = document.createElement('table')
        table.style.paddingTop = '20px'
        table.style.borderCollapse = 'collapse'
        table.style.maxWidth = '800px'
        div2.style.position = 'absolute'
        div2.style.zIndex = '1'
        title.appendChild(content)
        div2.appendChild(title)
        div2.appendChild(table)

        const tr = document.createElement('tr')
        for (const header of cfg.headers) {
            const th = document.createElement('th')
            th.innerText = header
            th.style.textAlign = 'left'
            th.style.padding = padding
            tr.appendChild(th)
        }
        table.appendChild(tr)

        let rowWithBg = true
        for (const entry of cfg.entries) {
            const tr = document.createElement('tr')
            table.appendChild(tr)

            const last = cfg.headers.length - 1
            for (let i = 0; i < cfg.headers.length; i++) {
                const td = document.createElement('td')
                td.style.padding = padding
                tr.appendChild(td)
                if (rowWithBg) {
                    td.style.background = '#00000010'
                    if (i == 0) {
                        td.style.borderTopLeftRadius = background_radius
                        td.style.borderBottomLeftRadius = background_radius
                    } else if (i == last) {
                        td.style.borderTopRightRadius = background_radius
                        td.style.borderBottomRightRadius = background_radius
                    }
                }
                if (i == 0) {
                    const a = document.createElement('a')
                    const linkText = document.createTextNode(entry.name)
                    a.appendChild(linkText)
                    a.title = entry.name
                    if (entry.href) {
                        a.href = entry.href
                    }
                    td.appendChild(a)
                } else {
                    const value = entry.values[i - 1]
                    if (value != null) {
                        td.innerText = value
                    }
                }
            }
            rowWithBg = !rowWithBg
        }
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
    logger.log(`Registering 'getShadersFn'.`)
    rustGenShadersFn = fn
}
host.exportGlobal({ registerGetShadersRustFn })

// ==============
// === FIXMES ===
// ==============

// FIXME: leftover from old script
// API[globalConfig.windowAppScopeConfigName] = config
