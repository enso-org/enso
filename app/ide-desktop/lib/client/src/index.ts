'use strict'

import assert from 'node:assert'
import buildCfg from '../../../build.json'
import Electron from 'electron'
import isDev from 'electron-is-dev'
import path from 'node:path'
// @ts-ignore
import * as Server from './server'
import util from 'node:util'
import { hideBin } from 'yargs/helpers'
import { project_manager_bundle } from '../paths.js'
import * as content from '../../content/src/config'
import chalk from 'chalk'
import * as security from './security'

import child_process, { SpawnOptions } from 'child_process'
import fss from 'node:fs'
import fsp from 'node:fs/promises'

import stringLength from 'string-length'

const yargs = require('yargs')

const execFile = util.promisify(child_process.execFile)

// ================
// === Defaults ===
// ================

class WindowSize {
    static separator = 'x'
    constructor(public width: number, public height: number) {}
    static default(): WindowSize {
        return new WindowSize(1380, 900)
    }
    static parse(arg: string): Error | WindowSize {
        const size = arg.split(WindowSize.separator)
        const widthStr = size[0]
        const heightStr = size[1]
        const width = widthStr ? parseInt(widthStr) : NaN
        const height = heightStr ? parseInt(heightStr) : NaN
        if (isNaN(width) || isNaN(height)) {
            return new Error(`Incorrect window size provided '${arg}'.`)
        } else {
            return new WindowSize(width, height)
        }
    }
    pretty(): string {
        return `${this.width}${WindowSize.separator}${this.height}`
    }
}

// =============
// === Paths ===
// =============

const root = Electron.app.getAppPath()
const resources = path.join(root, '..')
const projectManagerExecutable = path.join(
    resources,
    project_manager_bundle,
    // @ts-ignore
    // Placeholder for a bundler-provided define.
    PROJECT_MANAGER_IN_BUNDLE_PATH
)

// ==============
// === Naming ===
// ==============

function capitalizeFirstLetter(str: string): string {
    return str.charAt(0).toUpperCase() + str.slice(1)
}

function camelToKebabCase(str: string) {
    return str.replace(/([a-z])([A-Z])/g, '$1-$2').toLowerCase()
}

function camelCaseToTitle(str: string) {
    return capitalizeFirstLetter(str.replace(/([a-z])([A-Z])/g, '$1 $2'))
}

// ============
// === Help ===
// ============

const HELP_OPTION = 'help'
const FULL_HELP_OPTION = 'full-help'

let usage = chalk.bold(
    `
Enso ${buildCfg.version} command line interface.
Usage: enso [options] [--] [backend args]`
)

class Section {
    entries: any[] = []
    description = ''
    constructor(entries: any[] = []) {
        this.entries = entries
    }
}

/** We use custom help printer because Yargs has many issues:
 * 1. The option ordering is random and there is no way to enforce it.
 * 2. The option groups ordering is random and there is no way to enforce it.
 * 3. Every option has a `[type`] annotation and there is no API to disable it.
 * 4. There is no option to print commands with single dash instead of double-dash.
 * 5. Help coloring is not supported, and they do not want to support it:
 *    https://github.com/yargs/yargs/issues/251
 */
function printHelp(cfg: {
    config: typeof options
    groupsOrdering: string[]
    secondaryGroups: string[]
    fullHelp: boolean
}) {
    console.log(usage)
    const terminalWidth = yargs.terminalWidth()
    const indentSize = 0
    const optionPrefix = '-'
    const spacing = 2
    const sections: { [key: string]: Section } = {}
    for (const groupName of cfg.groupsOrdering) {
        if (cfg.fullHelp || !cfg.secondaryGroups.includes(groupName)) {
            sections[groupName] = new Section()
        }
    }
    let maxOptionLength = 0

    for (const [groupName, group] of Object.entries(cfg.config.groups)) {
        let section = sections[groupName]
        if (section == null) {
            section = new Section()
            sections[groupName] = section
        }
        section.description = group.description
        for (const option of group.optionsRecursive()) {
            const cmdOption = camelToKebabCase(option.qualifiedName())
            maxOptionLength = Math.max(maxOptionLength, stringLength(cmdOption))
            const entry = [cmdOption, option]
            section.entries.push(entry)
        }
    }

    for (const [optionName, option] of Object.entries(cfg.config.options)) {
        const cmdOption = camelToKebabCase(optionName)
        maxOptionLength = Math.max(maxOptionLength, stringLength(cmdOption))
        const entry = [cmdOption, option]
        const section = sections[option.name]
        if (section != null) {
            section.entries.unshift(entry)
        }
        // sections['global'].entries.push(entry)
    }

    const borderStyle = (s: string) => chalk.gray(chalk.bold(s))

    const leftWidth = maxOptionLength + indentSize + stringLength(optionPrefix) + spacing
    const rightWidth = terminalWidth - leftWidth

    for (const [groupName, section] of Object.entries(sections)) {
        console.log('\n\n')
        const groupTitle = chalk.bold(`${camelCaseToTitle(groupName)} Options `)
        console.log(groupTitle)
        const description = wordWrap(section.description, terminalWidth).join('\n')
        console.log(description)
        console.log()
        for (const [cmdOption, option] of section.entries) {
            if (cfg.fullHelp || option.primary) {
                const indent = ' '.repeat(indentSize)
                let left = indent + chalk.bold(chalk.green(optionPrefix + cmdOption))
                const spaces = ' '.repeat(leftWidth - stringLength(left))
                left = left + spaces

                let firstSentenceSplit = option.description.indexOf('. ')
                let firstSentence =
                    firstSentenceSplit == -1
                        ? option.description
                        : option.description.slice(0, firstSentenceSplit + 1)
                let otherSentences = option.description.slice(firstSentence.length)

                const def = option.defaultDescription ?? option.default
                let defaults = ''
                if (def != null && def !== '') {
                    defaults = ` Defaults to ${chalk.green(def)}.`
                }
                let description = firstSentence + defaults + chalk.gray(otherSentences)
                const lines = wordWrap(description, rightWidth).map(
                    line => line + ' '.repeat(rightWidth - stringLength(line))
                )
                const right = lines.join('\n' + ' '.repeat(leftWidth))
                console.log(left + right)
            }
        }
    }
}

function wordWrap(str: string, width: number): string[] {
    if (width <= 0) {
        return []
    }
    let line = ''
    const lines = []
    const inputLines = str.split('\n')
    for (const inputLine of inputLines) {
        for (let word of inputLine.split(' ')) {
            if (stringLength(word) > width) {
                if (line.length > 0) {
                    lines.push(line)
                    line = ''
                }
                const wordChunks = []
                while (stringLength(word) > width) {
                    wordChunks.push(word.slice(0, width))
                    word = word.slice(width)
                }
                wordChunks.push(word)
                for (const wordChunk of wordChunks) {
                    lines.push(wordChunk)
                }
            } else {
                if (stringLength(line) + stringLength(word) >= width) {
                    lines.push(line)
                    line = ''
                }
                if (line.length != 0) {
                    line += ' '
                }
                line += word
            }
        }
    }
    if (line) {
        lines.push(line)
    }
    return lines
}

// ==============
// === Config ===
// ==============

const options = content.options.merge(
    new content.Group({
        options: {
            window: new content.Option({
                default: true,
                description:
                    'Show the window. If set to false, only the server will be run. You can use another ' +
                    'client or a browser to connect to it.',
            }),
            server: new content.Option({
                default: true,
                description:
                    'Run the server. If set to false, you can connect to an existing server on the ' +
                    'provided `port`.',
            }),
            showElectronOptions: new content.Option({
                default: false,
                description:
                    'Show Electron options in the help. Should be used together with `-help`.',
            }),
            info: new content.Option({
                default: false,
                description: `Print the system debug info.`,
            }),
            version: new content.Option({
                default: false,
                description: `Print the version.`,
            }),
            help: new content.Option({
                default: false,
                description:
                    'Show the common configuration options help page. ' +
                    'To see all options, use `-full-help`.',
            }),
            fullHelp: new content.Option({
                default: false,
                description: 'Show all the configuration options help page.',
            }),
        },
        groups: {
            window: new content.Group({
                options: {
                    size: new content.Option({
                        default: WindowSize.default().pretty(),
                        description: `The initial window size.`,
                    }),
                    frame: new content.Option({
                        default: process.platform !== 'darwin',
                        defaultDescription: 'false on MacOS, true otherwise',
                        description: 'Draw window frame.',
                    }),
                    vibrancy: new content.Option({
                        default: false,
                        description: 'Use the vibrancy effect.',
                    }),
                },
            }),
            server: new content.Group({
                options: {
                    port: new content.Option({
                        default: 8080,
                        description: `Port to use. In case the port is unavailable, next free port will be found.`,
                    }),
                },
            }),

            performance: new content.Group({
                options: {
                    backgroundThrottling: new content.Option({
                        default: false,
                        description: 'Throttle animations when run in background.',
                    }),
                    loadProfile: new content.Option({
                        // FIXME
                        default: [],
                        description:
                            'Load a performance profile. For use with developer tools such as the `profiling-run-graph` entry point.',
                    }),
                    saveProfile: new content.Option({
                        default: '',
                        description: 'Record a performance profile and write to a file.',
                    }),
                    workflow: new content.Option({
                        default: '',
                        description:
                            'Specify a workflow for profiling. Must be used with -entry-point=profile.',
                    }),
                },
            }),

            engine: new content.Group({
                options: {
                    backend: new content.Option({
                        default: true,
                        description: 'Start the backend process.',
                    }),
                    projectManagerPath: new content.Option({
                        default: '',
                        description:
                            'Set the path of a local project manager to use for running projects',
                    }),
                },
            }),

            debug: new content.Group({
                options: {
                    verbose: new content.Option({
                        default: false,
                        description: `Increase logs verbosity. Affects both IDE and the backend.`,
                    }),
                    dev: new content.Option({
                        default: false,
                        description: 'Run the application in development mode.',
                    }),
                    devtron: new content.Option({
                        default: false,
                        description: 'Install the Devtron Developer Tools extension.',
                    }),
                },
            }),
            electron: new content.Group({
                options: {
                    // === Electron Options ===
                    // https://www.electronjs.org/docs/latest/api/command-line-switches

                    authServerWhitelist: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            'A comma-separated list of servers for which integrated authentication is ' +
                            'enabled.',
                    }),
                    authNegotiateDelegateWhitelist: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            'A comma-separated list of servers for which delegation of user credentials is ' +
                            "required. Without '*' prefix the URL has to match exactly.",
                    }),
                    disableNtlmV2: new content.Option({
                        hidden: true,
                        default: false,
                        description: 'Disables NTLM v2 for posix platforms, no effect elsewhere.',
                    }),
                    disableHttpCache: new content.Option({
                        hidden: true,
                        default: false,
                        description: 'Disables the disk cache for HTTP requests.',
                    }),
                    disableHttp2: new content.Option({
                        hidden: true,
                        default: false,
                        description: 'Disable HTTP/2 and SPDY/3.1 protocols.',
                    }),
                    disableRendererBackgrounding: new content.Option({
                        hidden: true,
                        default: false,
                        description:
                            "Prevents Chromium from lowering the priority of invisible pages' renderer " +
                            'processes.',
                    }),
                    diskCacheSize: new content.Option({
                        hidden: true,
                        default: 0,
                        description:
                            'Forces the maximum disk space to be used by the disk cache, in bytes.',
                    }),
                    enableLogging: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            "Prints Chromium's logging to stderr (or a log file, if provided as argument).",
                    }),
                    forceFieldtrials: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            'Field trials to be forcefully enabled or disabled. For example, ' +
                            "'WebRTC-Audio-Red-For-Opus/Enabled/'.",
                    }),
                    hostRules: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            'A comma-separated list of rules that control how hostnames are mapped. For ' +
                            "example, 'MAP * 127.0.0.1'.",
                    }),
                    hostResolverRules: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            "Like '--host-rules' but these rules only apply to the host resolver.",
                    }),
                    ignoreCertificateErrors: new content.Option({
                        hidden: true,
                        default: false,
                        description: 'Ignores certificate related errors.',
                    }),
                    ignoreConnectionsLimit: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            "Ignore the connections limit for domains list separated by ','.",
                    }),
                    jsFlags: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            'Specifies the flags passed to the Node.js engine. For example, ' +
                            '\'-electron-js-flags="--harmony_proxies --harmony_collections"\'.',
                    }),
                    lang: new content.Option({
                        hidden: true,
                        default: '',
                        description: 'Set a custom locale.',
                    }),
                    logFile: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            "If '-electron-enable-logging' is specified, logs will be written to the given path. " +
                            'The parent directory must exist.',
                    }),
                    logNetLog: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            'Enables net log events to be saved and writes them to the provided path.',
                    }),
                    logLevel: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            "Sets the verbosity of logging when used together with '-electron-enable-logging'. " +
                            "The argument should be one of Chrome's LogSeverities.",
                    }),
                    noProxyServer: new content.Option({
                        hidden: true,
                        default: false,
                        description:
                            "Don't use a proxy server and always make direct connections. Overrides " +
                            'any other proxy server flags that are passed.',
                    }),
                    noSandbox: new content.Option({
                        hidden: true,
                        default: false,
                        description:
                            'Disables the Chromium sandbox. Forces renderer process and Chromium helper ' +
                            'processes to run un-sandboxed. Should only be used for testing.',
                    }),
                    proxyBypassList: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            'Instructs Electron to bypass the proxy server for the given ' +
                            'semi-colon-separated list of hosts. This flag has an effect only if used in tandem ' +
                            "with '--proxy-server'. For example, " +
                            '\'--proxy-bypass-list "<local>;*.google.com;*foo.com;1.2.3.4:5678"\'.',
                    }),
                    proxyPacUrl: new content.Option({
                        hidden: true,
                        default: '',
                        description: 'Uses the PAC script at the specified url.',
                    }),
                    proxyServer: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            "Use a specified proxy server ('address:port'), which overrides the system " +
                            'setting. This switch only affects requests with HTTP protocol, including HTTPS and ' +
                            'WebSocket requests. It is also noteworthy that not all proxy servers support HTTPS ' +
                            'and WebSocket requests. The proxy URL does not support username and password ' +
                            'authentication per ' +
                            '[Chromium issue](https://bugs.chromium.org/p/chromium/issues/detail?id=615947).',
                    }),
                    remoteDebuggingPort: new content.Option({
                        hidden: true,
                        default: '',
                        description: 'Enables remote debugging over HTTP on the specified port.',
                    }),
                    v: new content.Option({
                        hidden: true,
                        default: 0,
                        description:
                            'Gives the default maximal active V-logging level; 0 is the default. Normally ' +
                            'positive values are used for V-logging levels. This switch only works when ' +
                            "'-electron-enable-logging' is also passed.",
                    }),
                    vmodule: new content.Option({
                        hidden: true,
                        default: '',
                        description:
                            'Gives the per-module maximal V-logging levels to override the value given by ' +
                            "'-electron-v'. E.g. 'my_module=2,foo*=3' would change the logging level for all code in " +
                            "source files 'my_module.*' and 'foo*.*'. Any pattern containing a forward or " +
                            'backward slash will be tested against the whole pathname and not only the module. ' +
                            "This switch only works when '-electron-enable-logging' is also passed.",
                    }),
                    force_high_performance_gpu: new content.Option({
                        hidden: true,
                        default: false,
                        description:
                            'Force using discrete GPU when there are multiple GPUs available.',
                    }),
                    force_low_power_gpu: new content.Option({
                        hidden: true,
                        default: false,
                        description:
                            'Force using integrated GPU when there are multiple GPUs available.',
                    }),
                },
            }),
        },
    })
)
options.groups.startup.options.platform.default = process.platform
options.groups.startup.options.platform.value = process.platform

// =====================
// === Option Parser ===
// =====================

let argv = hideBin(process.argv)

const yargOptions = options.optionsRecursive().reduce((opts: { [key: string]: any }, option) => {
    const yargsParam = Object.assign({}, option)
    // @ts-ignore
    yargsParam.requiresArg = ['string', 'array'].includes(yargsParam.type)
    // @ts-ignore
    yargsParam.default = undefined
    // @ts-ignore
    opts[camelToKebabCase(option.qualifiedName())] = yargsParam
    return opts
}, {})

let optParser = yargs()
    .version(false)
    .parserConfiguration({
        'short-option-groups': false,
        'dot-notation': false,
        // Makes all flags passed after '--' be one string.
        'populate--': true,
    })
    .strict()
    .options(yargOptions)

// === Parsing ===

console.log('argv:', argv)

let xargs = optParser.parse(argv, {}, (err: any, args: any, help: string) => {
    console.log('!!!', err, help)
    if (help) {
        printHelp({
            config: options,
            groupsOrdering: [],
            secondaryGroups: ['Electron Options'],
            fullHelp: args[FULL_HELP_OPTION],
        })
        process.exit()
    }
})

console.log('Parsed args:', xargs)

for (const option of options.optionsRecursive()) {
    const arg = xargs[camelToKebabCase(option.qualifiedName())]
    if (arg != null) {
        option.value = arg
        option.setByUser = true
    }
}

let windowSize = WindowSize.default()
const parsedWindowSize = WindowSize.parse(options.groups.window.options.size.value)

if (parsedWindowSize instanceof Error) {
    throw 'wrong window size'
} else {
    windowSize = parsedWindowSize
}

if (options.options.help.value || options.options.fullHelp.value) {
    printHelp({
        config: options,
        groupsOrdering: [],
        secondaryGroups: ['Electron Options'],
        fullHelp: options.options.fullHelp.value,
    })
    process.exit()
}

// ==================
// === Debug Info ===
// ==================

let versionInfo = {
    version: buildCfg.version,
    build: buildCfg.commit,
    electron: process.versions.electron,
    chrome: process.versions.chrome,
}

async function getDebugInfo() {
    let procMemInfo = await process.getProcessMemoryInfo()
    return {
        version: versionInfo,
        creation: process.getCreationTime(),
        perf: {
            cpu: process.getCPUUsage(),
        },
        memory: {
            heap: process.getHeapStatistics(),
            blink: process.getBlinkMemoryInfo(),
            process: procMemInfo,
            system: process.getSystemMemoryInfo(),
        },
        system: {
            platform: process.platform,
            arch: process.arch,
            version: process.getSystemVersion(),
        },
    }
}

async function printDebugInfo() {
    let info = await getDebugInfo()
    console.log(JSON.stringify(info, undefined, 4))
    process.exit()
}

// =======================
// === Project Manager ===
// =======================

function projectManagerPath() {
    let binPath = options.groups.engine.options.projectManagerPath.value || projectManagerExecutable
    let binExists = fss.existsSync(binPath)
    assert(binExists, `Could not find the project manager binary at ${binPath}.`)
    return binPath
}
/**
 * Executes the Project Manager with given arguments.
 *
 * Note that this function captures all the Project Manager output into a fixed
 * size buffer. If too much output is produced, it will fail and Project
 * Manager process will prematurely close.
 *
 * @param {string[]} args Project Manager command line arguments.
 * @returns Promise with captured standard output and error contents.
 */
async function execProjectManager(args: string[]) {
    let binPath = projectManagerPath()
    return await execFile(binPath, args).catch(function (err) {
        throw err
    })
}

/**
 * Spawn process with Project Manager,
 *
 * The standard output and error handles will be inherited, i.e. will be
 * redirected to the electron's app output and error handles. Input is piped
 * to this process, so it will not be closed, until this process finished.
 *
 * @param {string[]} args
 * @returns Handle to the spawned process.
 */
function spawnProjectManager(args: string[]) {
    console.log('Starting the backend process with the following options:', args)
    let binPath = projectManagerPath()
    let stdin: 'pipe' = 'pipe'
    let stdout: 'inherit' = 'inherit'
    let stderr: 'inherit' = 'inherit'
    let opts: SpawnOptions = {
        stdio: [stdin, stdout, stderr],
    }
    let out = child_process.spawn(binPath, args, opts)
    console.log(`Project Manager has been spawned, pid = ${out.pid}.`)
    out.on('exit', code => {
        console.log(`Project Manager exited with code ${code}.`)
    })
    return out
}

async function backendVersion() {
    if (options.groups.engine.options.backend.value) {
        return await execProjectManager(['--version']).then(t => t.stdout)
    }
}

// ============
// === Main ===
// ============
security.enableAll()

let hideInsteadOfQuit = false

let server = null
let mainWindow: null | Electron.BrowserWindow = null
let origin: null | string = null

function urlParamsFromObject(obj: { [key: string]: string }) {
    let params = []
    for (let key in obj) {
        let val = obj[key]
        params.push(`${key}=${val}`)
    }
    return params.join('&')
}

class App {
    async main() {
        // We catch all errors here. Otherwise, it might be possible that the app will run partially
        // and the user will not see anything.
        try {
            this.startBackend()
            await this.startContentServer()
            this.createWindow()
        } catch (err) {
            console.error('Failed to initialize the application, shutting down. Error:', err)
            Electron.app.quit()
        }
    }

    startBackend() {
        if (!options.groups.engine.options.backend.value) {
            console.log('The app is configured not to run the backend process.')
        } else {
            const dashArgsOpt = xargs['--']
            const dashArgs = dashArgsOpt ? dashArgsOpt : []
            const verboseArgs = options.groups.debug.options.verbose.value ? ['-vv'] : []
            const args = dashArgs.concat(verboseArgs)
            return spawnProjectManager(args)
        }
    }

    async startContentServer() {
        if (!options.options.server.value) {
            console.log('The app is configured not to run the content server.')
        } else {
            console.log('Starting the content server.')
            let serverCfg = new Server.Config({
                dir: root,
                port: options.groups.server.options.port.value,
                fallback: '/assets/index.html',
            })
            server = await Server.create(serverCfg)
            origin = `http://localhost:${server.config.port}`
        }
    }

    createWindow() {
        if (!options.options.window.value) {
            console.log('The app is configured not to create a window.')
        } else {
            const webPreferences = security.secureWebPreferences()
            webPreferences.preload = path.join(root, 'preload.cjs')
            webPreferences.sandbox = true
            webPreferences.backgroundThrottling =
                options.groups.performance.options.backgroundThrottling.value
            webPreferences.devTools = options.groups.debug.options.dev.value

            let windowPreferences: Electron.BrowserWindowConstructorOptions = {
                webPreferences,
                width: windowSize.width,
                height: windowSize.height,
                frame: options.groups.window.options.frame.value,
                transparent: false,
                titleBarStyle: 'default',
            }

            if (windowPreferences.frame === false && process.platform === 'darwin') {
                windowPreferences.titleBarStyle = 'hiddenInset'
            }

            if (options.groups.window.options.vibrancy.value) {
                windowPreferences.vibrancy = 'fullscreen-ui'
            }

            const window = new Electron.BrowserWindow(windowPreferences)
            window.setMenuBarVisibility(false)

            if (options.groups.debug.options.dev.value) {
                window.webContents.openDevTools()
            }

            const urlCfg: { [key: string]: string } = {}
            for (const option of options.optionsRecursive()) {
                if (option.setByUser) {
                    urlCfg[option.qualifiedName()] = String(option.value)
                }
            }

            Electron.ipcMain.on('error', (event, data) => console.error(data))

            // FIXME
            // let profilePromises = []
            // if (args.loadProfile) {
            //     profilePromises = args.loadProfile.map((path: string) => fsp.readFile(path, 'utf8'))
            // }
            // const profiles = Promise.all(profilePromises)
            // Electron.ipcMain.on('load-profiles', event => {
            //     profiles.then(profiles => {
            //         event.reply('profiles-loaded', profiles)
            //     })
            // })
            // if (args.saveProfile) {
            //     Electron.ipcMain.on('save-profile', (event, data) => {
            //         fss.writeFileSync(args.saveProfile, data)
            //     })
            // }
            // if (args.workflow) {
            //     // @ts-ignore
            //     urlCfg.testWorkflow = args.workflow
            // }

            Electron.ipcMain.on('quit-ide', () => {
                Electron.app.quit()
            })

            let params = urlParamsFromObject(urlCfg)
            let address = `${origin}?${params}`

            console.log(`Loading the window address ${address}`)
            window.loadURL(address)
            window.on('close', evt => {
                if (hideInsteadOfQuit) {
                    evt.preventDefault()
                    window.hide()
                }
            })
        }
    }
}

// ==============
// === Events ===
// ==============

function printVersion() {
    let indent = ' '.repeat(4)
    let maxNameLen = 0
    for (let name in versionInfo) {
        if (name.length > maxNameLen) {
            maxNameLen = name.length
        }
    }

    console.log('Frontend:')
    for (let name in versionInfo) {
        let label = capitalizeFirstLetter(name)
        let spacing = ' '.repeat(maxNameLen - name.length)
        // @ts-ignore
        console.log(`${indent}${label}:${spacing} ${versionInfo[name]}`)
    }

    console.log('')
    console.log('Backend:')
    backendVersion().then(backend => {
        if (!backend) {
            console.log(`${indent}No backend available.`)
        } else {
            let lines = backend.split(/\r?\n/)
            for (let line of lines) {
                console.log(`${indent}${line}`)
            }
        }
        process.exit()
    })
}

Electron.app.on('activate', () => {
    if (process.platform === 'darwin') {
        if (mainWindow != null) {
            mainWindow.show()
        } else {
            // FIXME
        }
    }
})

const app = new App()
Electron.app.whenReady().then(() => {
    if (options.options.version.value) {
        printVersion()
    } else if (options.options.info.value) {
        printDebugInfo()
    } else {
        app.main()
    }
})

if (process.platform === 'darwin') {
    hideInsteadOfQuit = true
    Electron.app.on('before-quit', function () {
        hideInsteadOfQuit = false
    })
}

// =================
// === Shortcuts ===
// =================

Electron.app.on('web-contents-created', (webContentsCreatedEvent, webContents) => {
    webContents.on('before-input-event', (beforeInputEvent, input) => {
        const { code, alt, control, shift, meta, type } = input
        if (type !== 'keyDown') {
            return
        }
        if (control && alt && shift && !meta && code === 'KeyI') {
            const focusedWindow = Electron.BrowserWindow.getFocusedWindow()
            if (focusedWindow) {
                focusedWindow.webContents.toggleDevTools()
                // FIXME: what if not
            }
        }
        if (control && alt && shift && !meta && code === 'KeyR') {
            const focusedWindow = Electron.BrowserWindow.getFocusedWindow()
            if (focusedWindow) {
                focusedWindow.reload()
                // FIXME: what if not
            }
        }

        let cmd_q = meta && !control && !alt && !shift && code === 'KeyQ'
        let ctrl_q = !meta && control && !alt && !shift && code === 'KeyQ'
        let alt_f4 = !meta && !control && alt && !shift && code === 'F4'
        let ctrl_w = !meta && control && !alt && !shift && code === 'KeyW'
        let quit_on_mac = process.platform === 'darwin' && (cmd_q || alt_f4)
        let quit_on_win = process.platform === 'win32' && (alt_f4 || ctrl_w)
        let quit_on_lin = process.platform === 'linux' && (alt_f4 || ctrl_q || ctrl_w)
        let quit = quit_on_mac || quit_on_win || quit_on_lin
        if (quit) {
            Electron.app.quit()
        }
    })
})

// =============================
// === Deprecations & Fixmes ===
// =============================

// FIXME Enable Metal backend on MacOS https://github.com/electron/electron/issues/22465

// TODO[WD] Windows vibrancy
// https://github.com/fstudio/clangbuilder/issues/39
// https://github.com/Microsoft/vscode/issues/32257
// https://github.com/arkenthera/electron-vibrancy/issues/21

// TODO[WD] Window corner radius
// https://github.com/electron/electron/issues/22542
