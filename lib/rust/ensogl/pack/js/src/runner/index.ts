/** @file Runner for the code generated by `ensogl-pack`. It is responsible for downloading and
 * compiling WASM, displaying loading and help screens, and configuring the application. */

import * as dom from 'runner/dom/dom'
import * as name from 'runner/name'
import * as log from 'runner/log'
import * as wasm from 'runner/wasm'
import * as config from 'runner/config'
import * as array from 'runner/data/array'
import * as debug from 'runner/debug'

import host from 'runner/host'
import { logger } from 'runner/log'
import { sortedWasmFunctions } from 'runner/wasm'

// ===============
// === Exports ===
// ===============

export { log }
export { config }
export type { LogLevel } from 'runner/log/logger'

export { logger, Logger, Consumer } from 'runner/log'
export { Param } from 'runner/config'

// ==============================
// === Files to be downloaded ===
// ==============================

/** Files that are downloaded from server during app startup. */
class Files<T> {
    /** Main JS file that is responsible for initializing and compiling WASM. */
    pkgJs: T
    /** Main WASM file that contains the compiled WASM code. */
    pkgWasm: T
    /** Precompiled shaders files. */
    shaders = new Shaders<T>()

    constructor(pkgJs: T, pkgWasm: T) {
        this.pkgJs = pkgJs
        this.pkgWasm = pkgWasm
    }

    async mapAndAwaitAll<S>(f: (t: T) => Promise<S>): Promise<Files<S>> {
        const mapped = await Promise.all(this.toArray().map(f))
        const out = this.fromArray(mapped)
        if (out != null) {
            return out
        } else {
            log.panic()
        }
    }

    /** Converts the structure fields to an array. */
    toArray(): T[] {
        return [this.pkgJs, this.pkgWasm, ...this.shaders.toArray()]
    }

    /** Assign array values to the structure fields. The elements order should be the same as the
     * output of the `toArray` function. */
    fromArray<S>(array: S[]): Files<S> | null {
        const [pkgJs, pkgWasm, ...shaders] = array
        if (pkgJs != null && pkgWasm != null) {
            const files = new Files<S>(pkgJs, pkgWasm)
            files.shaders = this.shaders.fromArray(shaders) ?? new Shaders()
            return files
        } else {
            return null
        }
    }
}

/** Mapping between a shader identifier and precompiled shader sources. */
class Shaders<T> {
    map = new Map<string, Shader<T>>()

    async mapAndAwaitAll<S>(f: (t: T) => Promise<S>): Promise<Shaders<S>> {
        const mapped = await Promise.all(this.toArray().map(f))
        const out = this.fromArray(mapped)
        if (out != null) {
            return out
        } else {
            log.panic()
        }
    }

    /** Converts the structure fields to an array. The shader names are not preserved. */
    toArray(): T[] {
        return Array.from(this.map.values()).flatMap(shader => shader.toArray())
    }

    /** Assign array values to the structure fields. The elements order should be the same as the
     * output of the `toArray` function. The shader names will be preserved and assigned to the
     * input values in order. */
    fromArray<S>(arr: S[]): Shaders<S> | null {
        const shaders = new Shaders<S>()
        const keys = Array.from(this.map.keys())
        const tuples = array.arrayIntoTuples(arr)
        if (tuples == null) {
            log.panic()
        } else {
            for (const [key, [vertex, fragment]] of array.zip(keys, tuples)) {
                const shader = new Shader(vertex, fragment)
                shaders.map.set(key, shader)
            }
            return shaders
        }
    }
}

/** Precompiled shader sources */
class Shader<T> {
    vertex: T
    fragment: T

    constructor(vertex: T, fragment: T) {
        this.vertex = vertex
        this.fragment = fragment
    }

    /** Converts the structure fields to an array. The shader names are not preserved. */
    toArray(): T[] {
        return [this.vertex, this.fragment]
    }
}

// ===========
// === App ===
// ===========

/** Preferred frame time for the `Scheduler`. */
const FRAME_TIME_MS = 16

/** A task scheduler used to run tasks in the next animation frame if the current animation frame is
 * running too long. */
class Scheduler {
    done: Promise<void>
    doneResolve: () => void = () => {}
    time: DOMHighResTimeStamp = 0
    tasks: (() => void)[] = []

    constructor() {
        this.done = new Promise(resolve => {
            this.doneResolve = resolve
        })
    }

    add(task: () => void) {
        this.tasks.push(task)
    }

    run(): Promise<void> {
        if (host.node) {
            for (const task of this.tasks) {
                task()
            }
            this.doneResolve()
        } else {
            this.onFrame()
        }
        return this.done
    }

    onFrame() {
        for (;;) {
            const time = window.performance.now()
            const delta = time - this.time
            if (delta > FRAME_TIME_MS) {
                break
            }
            const task = this.tasks.shift()
            if (task != null) {
                task()
            } else {
                this.doneResolve()
                break
            }
        }
        if (this.tasks.length === 0) {
            this.doneResolve()
        } else {
            this.time = window.performance.now()
            window.requestAnimationFrame(this.onFrame.bind(this))
        }
    }
}

// ===========
// === App ===
// ===========

/** The main application class. */
export class App {
    packageInfo: debug.PackageInfo
    config: config.Config
    wasm: any = null
    loader: wasm.Loader | null = null
    shaders: Shaders<string> | null = null
    wasmFunctions: string[] = []
    beforeMainEntryPoints = new Map<string, wasm.BeforeMainEntryPoint>()
    mainEntryPoints = new Map<string, wasm.EntryPoint>()
    progressIndicator: wasm.ProgressIndicator | null = null
    initialized = false

    constructor(opts?: {
        configExtension?: config.ExternalConfig
        packageInfo?: Record<string, string>
        config?: Record<string, any>
    }) {
        this.packageInfo = new debug.PackageInfo(opts?.packageInfo ?? {})
        this.config = new config.Config(opts?.configExtension)
        const unrecognizedParams = this.config.resolve([opts?.config, host.urlParams()])
        if (unrecognizedParams) {
            this.config.print()
            this.showConfigOptions(unrecognizedParams)
        } else {
            this.initBrowser()
            this.initialized = true
        }
        // Export the app to a global variable, so Rust can access it.
        host.exportGlobal({ ensoglApp: this })
    }

    /** Registers the Rust function that extracts the shader definitions. */
    registerGetShadersRustFn(fn: GetShadersFn) {
        logger.log(`Registering 'getShadersFn'.`)
        rustGetShadersFn = fn
    }

    /** Registers the Rust function that injects the shader definitions. */
    registerSetShadersRustFn(fn: SetShadersFn) {
        logger.log(`Registering 'setShadersFn'.`)
        rustSetShadersFn = fn
    }

    /** Log the message on the remote server. */
    remoteLog(message: string, data: any) {
        // TODO: Implement remote logging. This should be done after cloud integration.
    }

    /** Initialize the browser. Set the background color, print user-facing warnings, etc. */
    initBrowser() {
        if (host.browser) {
            this.styleRoot()
            dom.disableContextMenu()
            if (this.config.params.debug.value) {
                logger.log('Application is run in debug mode. Logs will not be hidden.')
            } else {
                this.printScamWarning()
                log.router.hideLogs()
            }
        }
    }

    /** Set the background color of the root element. */
    styleRoot() {
        const root = document.getElementById('root')
        if (root != null) {
            root.style.backgroundColor = 'rgb(234,238,241)'
        }
    }

    /** Runs the application. It will initialize DOM elements, display a loader, run before main
     * entry points and the main entry point. It will also list available entry points if the
     * provided entry point is missing. */
    async run(): Promise<void> {
        if (!this.initialized) {
            logger.log("App wasn't initialized properly. Skipping run.")
        } else {
            this.config.print()
            await this.loadAndInitWasm()
            await this.runEntryPoints()
        }
    }

    /** Compiles and runs the downloaded WASM file. */
    async compileAndRunWasm(pkgJs: string, wasm: Buffer | Response): Promise<unknown> {
        return await log.Task.asyncRunNoGroup<unknown>('WASM compilation', async () => {
            /* eslint @typescript-eslint/no-implied-eval: "off" */
            /* eslint @typescript-eslint/no-unsafe-assignment: "off" */
            const snippetsFn: any = Function(
                // A hack to make Emscripten output load correctly (e.g. 'msdf-gen'). Emscripten
                // generates a code which overrides `module.exports` after checking if the code
                // is run in node. The check is performed by checking the values of `process`.
                // Setting it to something else prevents the code from running.
                `const process = "Overridden to prevent Emscripten from redefining module.exports."
                 const module = {}
                 ${pkgJs}
                 return module.exports`
            )()
            const out: unknown = await snippetsFn.init(wasm)
            if (this.config.params.enableSpector.value) {
                /* eslint @typescript-eslint/no-unsafe-member-access: "off" */
                /* eslint @typescript-eslint/no-unsafe-call: "off" */
                if (host.browser) {
                    const spectorModule: unknown = snippetsFn.spector()
                    console.log(spectorModule)
                    // @ts-ignore
                    const spector = new spectorModule.Spector()
                    // @ts-ignore
                    spector.displayUI()
                }
            }
            return out
        })
    }

    /** Download and load the WASM to memory. */
    async loadWasm() {
        const loader = new wasm.Loader(this.config)

        const shadersUrl = this.config.params.shadersUrl.value
        const shadersNames = await log.Task.asyncRunCollapsed(
            'Downloading shaders list.',
            async () => {
                const shadersListResponse = await fetch(`${shadersUrl}/list.txt`)
                const shadersList = await shadersListResponse.text()
                return shadersList.split('\n').filter(line => line.length > 0)
            }
        )

        const files = new Files(
            this.config.params.pkgJsUrl.value,
            this.config.params.pkgWasmUrl.value
        )
        for (const mangledName of shadersNames) {
            const unmangledName = name.unmangle(mangledName)
            const vertexUrl = `${shadersUrl}/${mangledName}.vertex.glsl`
            const fragmentUrl = `${shadersUrl}/${mangledName}.fragment.glsl`
            files.shaders.map.set(unmangledName, new Shader(vertexUrl, fragmentUrl))
        }

        const responses = await files.mapAndAwaitAll(url => fetch(url))
        const responsesArray = responses.toArray()
        loader.load(responsesArray)
        const downloadSize = loader.showTotalBytes()
        const task = log.Task.startCollapsed(`Downloading application files (${downloadSize}).`)
        void loader.done.then(() => task.end())

        for (const file of files.toArray()) {
            logger.log(`Downloading '${file}'.`)
        }

        const pkgJs = await responses.pkgJs.text()
        this.loader = loader
        this.wasm = await this.compileAndRunWasm(pkgJs, responses.pkgWasm)
        this.shaders = await responses.shaders.mapAndAwaitAll(t => t.text())
    }

    /** Loads the WASM binary and its dependencies. After the files are fetched, the WASM module is
     * compiled and initialized. */
    async loadAndInitWasm() {
        await this.loadWasm()
        this.wasmFunctions = wasm.sortedWasmFunctions(this.wasm)
        this.beforeMainEntryPoints = wasm.BeforeMainEntryPoint.fromNames(this.wasmFunctions)
        this.mainEntryPoints = wasm.EntryPoint.fromNames(this.wasmFunctions)
        this.packageInfo.display()
    }

    /** Run all before main entry points. See the docs of `wasm.entryPoint` to learn more. */
    async runBeforeMainEntryPoints(): Promise<void> {
        const count = this.beforeMainEntryPoints.size
        const scheduler = new Scheduler()
        if (this.beforeMainEntryPoints.size) {
            for (const entryPoint of this.beforeMainEntryPoints.values()) {
                scheduler.add(() => {
                    log.Task.runTimed(`Running entry point '${entryPoint.displayName()}'.`, () => {
                        const fn = this.wasm[entryPoint.name()]
                        if (fn != null) {
                            fn()
                        } else {
                            logger.internalError(`Entry point not found.`)
                        }
                    })
                })
            }
        }
        const [time] = await log.Task.asyncRunCollapsedTimed(
            `Running ${count} before main entry points.`,
            async () => {
                return await scheduler.run()
            }
        )
        this.checkBeforeMainEntryPointsTime(time)
    }

    /** Check whether the time needed to run before main entry points is reasonable. Print a warning
     * message otherwise. */
    checkBeforeMainEntryPointsTime(time: number) {
        if (time > this.config.params.maxBeforeMainEntryPointsTimeMs.value) {
            logger.error(
                `Entry points took ${time} milliseconds to run. This is too long. ` +
                    'Before main entry points should be used for fast initialization only.'
            )
        }
    }

    async showProgressIndicator() {
        this.progressIndicator = new wasm.ProgressIndicator(this.config, false)
        this.progressIndicator.set(0.5)
        await Promise.all([
            this.progressIndicator.animateShow(),
            this.progressIndicator.animateShowLogo(),
            this.progressIndicator.animateProgress()
        ])
    }

    async hideProgressIndicator() {
        if (this.progressIndicator) {
            await this.progressIndicator.animateHide()
            this.progressIndicator.destroy()
            this.progressIndicator = null
        }
    }

    /** Run both before main entry points and main entry point. */
    async runEntryPoints() {
        const entryPointName = this.config.params.entry.value
        const entryPoint = this.mainEntryPoints.get(entryPointName)
        if (entryPoint) {
            await this.runBeforeMainEntryPoints()
            if (this.shaders) this.setShaders(this.shaders.map)
            if (this.loader) this.loader.destroy()
            logger.log(`Running the main entry point '${entryPoint.displayName()}'.`)
            const fn = this.wasm[entryPoint.name()]
            if (fn != null) {
                fn()
            } else {
                logger.internalError(`Entry point not found.`)
            }
        } else {
            if (this.loader) this.loader.destroy()
            this.showEntryPointSelector(entryPointName)
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
            /* eslint @typescript-eslint/no-unsafe-assignment: "off" */
            const docsFn = this.wasm[entryPoint.docsFnName()]
            let description = 'No description.'
            if (docsFn) {
                const rustDocs = docsFn()
                if (rustDocs) {
                    description = rustDocs
                }
            }
            const href = '?entry=' + entryPoint.strippedName
            return new debug.HelpScreenEntry(entryPoint.strippedName, [description], href)
        })

        const headers = ['Name', 'Description']
        new debug.HelpScreen().display({ title, headers, entries })
    }

    showConfigOptions(unknownConfigOptions?: string[]) {
        logger.log('Showing config options help screen.')
        const msg = unknownConfigOptions
            ? `Unknown config options: '${unknownConfigOptions.join(', ')}'. `
            : ''
        const title = msg + 'Available options:'
        const entries = Array.from(Object.entries(this.config.params)).map(([key, option]) => {
            return new debug.HelpScreenEntry(key, [option.description, String(option.default)])
        })
        const headers = ['Name', 'Description', 'Default']
        new debug.HelpScreen().display({ title, headers, entries })
    }

    /** Print the warning for the end user that they should not copy any code to the console. */
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

    /* Get not optimized shaders from WASM. */
    getShaders(): Map<string, { vertex: string; fragment: string }> | null {
        return log.Task.run('Getting shaders from Rust.', () => {
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

    /* Set optimized shaders in WASM. */
    setShaders(map: Map<string, { vertex: string; fragment: string }>) {
        log.Task.runCollapsed(`Sending ${map.size} shaders to Rust.`, () => {
            if (!rustSetShadersFn) {
                logger.error('The Rust shader injection function was not registered.')
            } else {
                logger.log(`Setting ${map.size} shader definitions.`)
                rustSetShadersFn(map)
            }
        })
    }
}

// ==========================
// === App Initialization ===
// ==========================

type GetShadersFn = () => Map<string, { vertex: string; fragment: string }>
type SetShadersFn = (map: Map<string, { vertex: string; fragment: string }>) => void

let rustGetShadersFn: null | GetShadersFn = null
let rustSetShadersFn: null | SetShadersFn = null

/** Registers the Rust function that extracts the shader definitions. */
function registerGetShadersRustFn(fn: GetShadersFn) {
    logger.log(`Registering 'getShadersFn'.`)
    rustGetShadersFn = fn
}

/** Registers the Rust function that injects the shader definitions. */
function registerSetShadersRustFn(fn: SetShadersFn) {
    logger.log(`Registering 'setShadersFn'.`)
    rustSetShadersFn = fn
}

host.exportGlobal({ registerGetShadersRustFn, registerSetShadersRustFn })
