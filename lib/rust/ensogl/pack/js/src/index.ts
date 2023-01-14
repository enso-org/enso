import * as html_utils from 'dom/dom'
import * as fs from 'system/fs'
import * as path from 'path'
import * as name from 'name'
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

function arrayIntoTuples<T>(arr: T[]): [T, T][] {
    const tuples: [T, T][] = []
    for (let i = 0; i < arr.length; i += 2) {
        const elem1 = arr[i]
        const elem2 = arr[i + 1]
        if (elem1 != null && elem2 != null) {
            tuples.push([elem1, elem2])
        }
    }
    return tuples
}

function zip<T, S>(arr1: T[], arr2: S[]): [T, S][] {
    // @ts-ignore
    return [...arr1].map((_, c) => [arr1, arr2].map(row => row[c]))
}

class Shader<T> {
    vertex: T
    fragment: T

    constructor(vertex: T, fragment: T) {
        this.vertex = vertex
        this.fragment = fragment
    }

    toArray(): T[] {
        return [this.vertex, this.fragment]
    }
}

class Shaders<T> {
    map: Map<string, Shader<T>> = new Map()

    async mapAndAwaitAll<S>(f: (t: T) => Promise<S>): Promise<Shaders<S>> {
        const mapped = await Promise.all(this.toArray().map(f))
        const out = this.fromArray(mapped)
        if (out != null) {
            return out
        } else {
            throw 'Internal error.'
        }
    }

    toArray(): T[] {
        return Array.from(this.map.values()).flatMap(shader => shader.toArray())
    }

    fromArray<S>(array: S[]): Shaders<S> | null {
        const shaders: Shaders<S> = new Shaders()
        const keys = Array.from(this.map.keys())
        for (const [key, [vertex, fragment]] of zip(keys, arrayIntoTuples(array))) {
            const shader = new Shader(vertex, fragment)
            shaders.map.set(key, shader)
        }
        return shaders
    }
}

class Files<T> {
    mainJs: T
    mainWasm: T
    shaders: Shaders<T> = new Shaders()

    constructor(mainJs: T, mainWasm: T) {
        this.mainJs = mainJs
        this.mainWasm = mainWasm
    }

    async mapAndAwaitAll<S>(f: (t: T) => Promise<S>): Promise<Files<S>> {
        const mapped = await Promise.all(this.toArray().map(f))
        const out = this.fromArray(mapped)
        if (out != null) {
            return out
        } else {
            throw 'Internal error.'
        }
    }

    toArray(): T[] {
        return [this.mainJs, this.mainWasm, ...this.shaders.toArray()]
    }

    fromArray<S>(array: S[]): Files<S> | null {
        const [mainJs, mainWasm, ...shaders] = array
        if (mainJs != null && mainWasm != null) {
            const files: Files<S> = new Files(mainJs, mainWasm)
            files.shaders = this.shaders.fromArray(shaders) || new Shaders()
            return files
        } else {
            return null
        }
    }
}

/** Loads the WASM binary and its dependencies. If it's run in the browser, the files will be
 * downloaded from a server and a loading progress indicator will be shown. If it's run in node, the
 * files will be read from disk. After the files are fetched, the WASM module is compiled and
 * initialized. */
async function loadWasm(config: Config) {
    if (host.browser) {
        const task = Task.startCollapsed(`Downloading application files.`)
        const loader = new Loader(config)
        loader.done.then(() => task.end())

        const shadersUrl = config.shadersUrl.value
        const shadersNames = await Task.asyncWith('Downloading shaders list.', async () => {
            const shadersListResponse = await fetch(`${shadersUrl}/list.txt`)
            const shadersList = await shadersListResponse.text()
            return shadersList.split('\n').filter(line => line.length > 0)
        })

        const files = new Files(config.mainJsUrl.value, config.mainWasmUrl.value)
        for (const mangledName of shadersNames) {
            const unmangledName = name.unmangle(mangledName)
            const vertexUrl = `${shadersUrl}/${mangledName}.vertex.glsl`
            const fragmentUrl = `${shadersUrl}/${mangledName}.fragment.glsl`
            files.shaders.map.set(unmangledName, new Shader(vertexUrl, fragmentUrl))
        }

        const responses = await files.mapAndAwaitAll(url => fetch(url))
        const responsesArray = responses.toArray()
        loader.load(responsesArray)

        for (const file of files.toArray()) {
            logger.log(`Downloading '${file}'.`)
        }

        // FIXME:
        // @ts-ignore
        const downloadSize = loader.showTotalBytes()

        const mainJs = await responses.mainJs.text()
        const wasm = await compileAndRunWasm(mainJs, responses.mainWasm)
        const shaders = await responses.shaders.mapAndAwaitAll(t => t.text())
        return { wasm, loader, shaders }
    } else {
        const mainJsUrl = path.join(__dirname, config.mainJsUrl.value)
        const mainWasmUrl = path.join(__dirname, config.mainWasmUrl.value)
        const mainJs = await fs.readFile(mainJsUrl, 'utf8')
        const mainWasm = await fs.readFile(mainWasmUrl)
        const wasm = await compileAndRunWasm(mainJs, mainWasm)
        const loader = null
        return { wasm, loader, shaders: null }
    }
}

/** Compiles and runs the downloaded WASM file. */
async function compileAndRunWasm(mainJs: string, wasm: Buffer | Response): Promise<any> {
    return await Task.asyncNoGroupWith('WASM compilation', async () => {
        const snippetsFn = Function(
            `const module = {}
             ${mainJs}
             module.exports.init = pkg_default
             return module.exports`
        )()
        return await snippetsFn.init(wasm)
    })
}

// ===========
// === App ===
// ===========

/** The main application class. */
export class App {
    args: Args = new Args()
    packageInfo: PackageInfo
    config: Config
    wasm: any = null
    loader: Loader | null = null
    logger: Logger
    shaders: Shaders<string> | null = null
    wasmFunctions: string[] = []
    beforeMainEntryPoints: Map<string, EntryPoint> = new Map()
    mainEntryPoints: Map<string, EntryPoint> = new Map()
    initialized = false

    constructor(opts?: { configExtension?: ExternalConfig; packageInfo: any; config?: object }) {
        this.packageInfo = new PackageInfo(opts?.packageInfo ?? {})
        this.config = new Config()
        if (opts?.configExtension) {
            this.config.extend(opts.configExtension)
        }

        this.logger = logger

        if (host.node) {
            this.args = parseArgs()
        } else {
            this.initBrowser()
        }
        const inputConfig = opts?.config ?? {}
        const unrecognizedParams = this.config.resolve({
            overrides: [inputConfig, host.urlParams()],
        })
        if (unrecognizedParams) {
            this.printResolvedConfig()
            this.showConfigOptions(unrecognizedParams)
        } else {
            this.initialized = true
        }
    }

    printResolvedConfig() {
        logger.log(`Resolved config:`, this.config.strigifiedKeyValueMap())
    }

    /** Runs the application. If it is run in the browser, it will initialize DOM elements, display
     * a loader, and list of entry points if the provided entry point is missing. If it is run in
     * node, it will run before main entry points and then the provided command. */
    async run(): Promise<void> {
        if (!this.initialized) {
            logger.log("App wasn't initialized properly. Skipping run.")
        } else {
            if (host.browser) {
                this.printResolvedConfig()
                await this.loadWasm()
                this.runEntryPoints()
            } else {
                const extractShadersPath = this.args.extractShaders.value
                if (extractShadersPath) {
                    await Task.asyncWith('Running the program.', async () => {
                        this.printResolvedConfig()
                        await this.loadWasm()
                        this.runBeforeMainEntryPoints()
                        await this.extractShaders(extractShadersPath)
                    })
                } else {
                    this.args.printHelpAndExit(1)
                }
            }
        }
    }

    async loadWasm() {
        const { wasm, loader, shaders } = await loadWasm(this.config)
        this.wasm = wasm
        this.loader = loader
        this.shaders = shaders
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
            if (this.shaders) setShaders(this.shaders.map)
            if (this.loader) this.loader.destroy()
            logger.log(`Running the main entry point: ${entryPoint.displayName()}.`)
            this.wasm[entryPoint.name()]()
        } else {
            if (this.loader) this.loader.destroy()
            this.showEntryPointSelector(entryPointName)
        }
    }

    async extractShaders(target: string) {
        await Task.asyncWith('Extracting shaders code.', async () => {
            const shadersMap = getShaders()
            if (shadersMap) {
                await Task.asyncWith(`Writing shaders to '${target}'.`, async () => {
                    await fs.rm(target, { recursive: true, force: true })
                    await fs.mkdir(target)
                    const fileNames = []
                    for (const [codePath, code] of shadersMap) {
                        const fileName = name.mangle(codePath)
                        const filePath = path.join(target, fileName)
                        await fs.writeFile(`${filePath}.vertex.glsl`, code.vertex)
                        await fs.writeFile(`${filePath}.fragment.glsl`, code.fragment)
                        fileNames.push(fileName)
                    }
                    const fileListPath = path.join(target, 'list.txt')
                    await fs.writeFile(fileListPath, fileNames.join('\n'))
                })
            }
        })
    }

    initBrowser() {
        if (host.browser) {
            this.styleRoot()
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

    styleRoot() {
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
        const backgroundRadius = '8px'
        const div = html_utils.newTopLevelDiv()
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
                        td.style.borderTopLeftRadius = backgroundRadius
                        td.style.borderBottomLeftRadius = backgroundRadius
                    } else if (i == last) {
                        td.style.borderTopRightRadius = backgroundRadius
                        td.style.borderBottomRightRadius = backgroundRadius
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

type GetShadersFn = () => Map<string, { vertex: string; fragment: string }>
type SetShadersFn = (map: Map<string, { vertex: string; fragment: string }>) => void

let rustGetShadersFn: null | GetShadersFn = null
let rustSetShadersFn: null | SetShadersFn = null

function registerGetShadersRustFn(fn: GetShadersFn) {
    logger.log(`Registering 'getShadersFn'.`)
    rustGetShadersFn = fn
}

function registerSetShadersRustFn(fn: SetShadersFn) {
    logger.log(`Registering 'setShadersFn'.`)
    rustSetShadersFn = fn
}

function getShaders(): Map<string, { vertex: string; fragment: string }> | null {
    return Task.with('Getting shaders from Rust.', () => {
        if (!rustGetShadersFn) {
            logger.error('The Rust shader extraction function was not registered.')
            return null
        } else {
            const result = rustGetShadersFn()
            logger.log(`Got ${result.size} shader definitions.`)
            return result
        }
    })
}

function setShaders(map: Map<string, { vertex: string; fragment: string }>) {
    Task.with('Sending shaders to Rust.', () => {
        if (!rustSetShadersFn) {
            logger.error('The Rust shader injection function was not registered.')
        } else {
            logger.log(`Setting ${map.size} shader definitions.`)
            rustSetShadersFn(map)
        }
    })
}

host.exportGlobal({ registerGetShadersRustFn, registerSetShadersRustFn })

if (host.node) {
    const app = new App()
    app.run()
}

// ==============
// === FIXMES ===
// ==============

// FIXME: leftover from old script
// API[globalConfig.windowAppScopeConfigName] = config
