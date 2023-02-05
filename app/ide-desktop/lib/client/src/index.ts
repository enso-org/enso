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
import remoteMain from '@electron/remote/main/index.js'
import { project_manager_bundle } from '../paths.js'
import * as content from '../../content/src/config'
import chalk from 'chalk'

import child_process, { SpawnOptions } from 'child_process'
import fss from 'node:fs'
import fsp from 'node:fs/promises'

import stringLength from 'string-length'

const yargs = require('yargs')

const execFile = util.promisify(child_process.execFile)

// ================
// === Defaults ===
// ================

let windowSize = {
    width: 1380,
    height: 900,
}

/** The list of hosts that the app can access. They are required for user authentication to work. */
const trustedHosts = [
    'enso-org.firebaseapp.com',
    'accounts.google.com',
    'accounts.youtube.com',
    'github.com',
]

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

/** We use custom help printer because Yargs has many issues:
 * 1. The option ordering is random and there is no way to enforce it.
 * 2. The option groups ordering is random and there is no way to enforce it.
 * 3. Every option has a `[type`] annotation and there is no API to disable it.
 * 4. There is no option to print commands with single dash instead of double-dash.
 * 5. Help coloring is not supported, and they do not want to support it:
 *    https://github.com/yargs/yargs/issues/251
 */
function printHelp(cfg: {
    config: typeof content.options
    groupsOrdering: string[]
    secondaryGroups: string[]
    fullHelp: boolean
}) {
    console.log(usage)
    const terminalWidth = yargs.terminalWidth()
    const indentSize = 2
    const optionPrefix = '-'
    const spacing = 2
    const groups: { [key: string]: any[] } = {}
    for (const groupName of cfg.groupsOrdering) {
        if (cfg.fullHelp || !cfg.secondaryGroups.includes(groupName)) {
            groups[groupName] = []
        }
    }
    let maxOptionLength = 0
    for (const [key, option] of Object.entries(cfg.config)) {
        const group = option.group || 'Other Options'
        if (cfg.fullHelp || !cfg.secondaryGroups.includes(group)) {
            const cmdOption = camelToKebabCase(option.qualifiedName())
            maxOptionLength = Math.max(maxOptionLength, stringLength(cmdOption))
            if (!groups[group]) {
                groups[group] = []
            }
            groups[group].push([cmdOption, option])
        }
    }
    const leftWidth = maxOptionLength + indentSize + stringLength(optionPrefix) + spacing
    const rightWidth = terminalWidth - leftWidth

    for (const [group, options] of Object.entries(groups)) {
        console.log(chalk.bold(`\n\n${group}:`))
        for (const [cmdOption, option] of options) {
            if (cfg.fullHelp || option.primary) {
                const indent = ' '.repeat(indentSize)
                let left = indent + chalk.bold(chalk.ansi256(191)(optionPrefix + cmdOption))
                const spaces = ' '.repeat(leftWidth - stringLength(left))
                left += spaces

                let firstSentenceSplit = option.description.indexOf('. ')
                let firstSentence =
                    firstSentenceSplit == -1
                        ? option.description
                        : option.description.slice(0, firstSentenceSplit + 1)
                let otherSentences = option.description.slice(firstSentence.length)

                const def = option.defaultDescription ?? option.default
                let defaults = def == null ? '' : ` Defaults to ${chalk.ansi256(191)(def)}.`
                let description = firstSentence + defaults + chalk.ansi256(245)(otherSentences)
                const right = wordWrap(description, rightWidth).join('\n' + ' '.repeat(leftWidth))
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

// @ts-ignore
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
                        default: `${windowSize.width}x${windowSize.height}`,
                        description: `The initial window size.`,
                    }),
                    frame: new content.Option({
                        default: true,
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
                        default: Server.DEFAULT_PORT,
                        description: `Port to use. In case the port is unavailable, next free port will be found.`,
                    }),
                },
            }),
            startup: new content.Group({
                options: {
                    project: new content.Option({
                        default: null,
                        description:
                            'Project name to open on startup. If not provided, the welcome screen will be ' +
                            'displayed.',
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
                    backendPath: new content.Option({
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
            style: new content.Group({
                options: {
                    // @ts-ignore
                    nodeLabels: (content.Option<boolean> = new content.Option({
                        // @ts-ignore
                        type: 'boolean',
                        default: true,
                        description: 'Show node labels.',
                    })),
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
                        default: null,
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
                        hidden: false,
                        default: null,
                        description: 'Disables the disk cache for HTTP requests.',
                    }),
                    disableHttp2: new content.Option({
                        hidden: false,
                        default: null,
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

// =============
// === Utils ===
// =============

// =====================
// === Option Parser ===
// =====================

let argv = hideBin(process.argv)

const yargOptions = options.optionsRecursive().reduce((opts: { [key: string]: any }, option) => {
    const yargsParam = Object.assign({}, option)
    yargsParam.requiresArg = ['string', 'array'].includes(yargsParam.type)
    yargsParam.default = undefined
    const group = yargsParam.group ? yargsParam.group + '.' : ''
    opts[group + camelToKebabCase(option.qualifiedName())] = yargsParam
    return opts
}, {})

let optParser = yargs(argv)
    .scriptName('')
    .version(false)
    // Makes all flags passed after '--' be one string.
    .parserConfiguration({ 'short-option-groups': false, 'populate--': true })
    .showHidden('show-electron-options', 'Show Electron options.')
    .strict()
    .wrap(yargs.terminalWidth())
    .options(yargOptions)

// === Parsing ===

let args = optParser.parse(argv, {}, (err: any, args: any, help: string) => {
    if (help) {
        printHelp({
            options,
            groupsOrdering: [],
            secondaryGroups: ['Electron Options'],
            fullHelp: args[FULL_HELP_OPTION],
        })
        process.exit()
    }
})

for (const key of Object.keys(config)) {
    if (args[key] !== undefined) {
        // @ts-ignore
        config[key].value = args[key]
        // @ts-ignore
        config[key].setByUser = true
    }
}

if (config.help.value || config.fullHelp.value) {
    printHelp({
        config,
        groupsOrdering: [],
        secondaryGroups: ['Electron Options'],
        fullHelp: config.fullHelp.value,
    })
    process.exit()
}

console.log('DONE')
throw 'break'

// Note: this is a conditional default to avoid issues with some window managers affecting
// interactions at the top of a borderless window. Thus, we want borders on Win/Linux and
// borderless on Mac. See https://github.com/enso-org/ide/issues/1101 and
// https://github.com/electron/electron/issues/3647 for details.
if (args.frame === undefined) {
    args.frame = process.platform !== 'darwin'
}

if (args.theme === undefined) {
    args.theme = 'light'
}

if (args.windowSize) {
    let size = args.windowSize.split('x')
    let width = parseInt(size[0])
    let height = parseInt(size[1])
    if (isNaN(width) || isNaN(height)) {
        console.error(`Incorrect window size provided '${args.windowSize}'.`)
    } else {
        windowSize.width = width
        windowSize.height = height
    }
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

// ================
// === Security ===
// ================

// === WebView Security ===

/// A WebView created in a renderer process that does not have Node.js integration enabled will not
/// be able to enable integration itself. However, a WebView will always create an independent
/// renderer process with its own webPreferences. It is a good idea to control the creation of new
/// <webview> tags from the main process and to verify that their webPreferences do not disable
/// security features. Follow the link to learn more:
/// https://www.electronjs.org/docs/tutorial/security#11-verify-webview-options-before-creation
function secureWebPreferences(webPreferences?: Electron.WebPreferences) {
    if (!webPreferences) {
        webPreferences = {}
    }
    delete webPreferences.preload
    delete webPreferences.nodeIntegration
    delete webPreferences.nodeIntegrationInWorker
    delete webPreferences.webSecurity
    delete webPreferences.allowRunningInsecureContent
    delete webPreferences.experimentalFeatures
    delete webPreferences.enableBlinkFeatures
    delete webPreferences.contextIsolation
    return webPreferences
}

let urlWhitelist: string[] = []
Electron.app.on('web-contents-created', (event, contents) => {
    contents.on('will-attach-webview', (event, webPreferences, params) => {
        secureWebPreferences(webPreferences)
        if (!urlWhitelist.includes(params.src)) {
            console.error(`Blocked the creation of WebView pointing to '${params.src}'`)
            event.preventDefault()
        }
    })
})

// === Prevent Navigation ===

/// Navigation is a common attack vector. If an attacker can convince your app to navigate away from
/// its current page, they can possibly force your app to open web sites on the Internet. Follow the
/// link to learn more:
/// https://www.electronjs.org/docs/tutorial/security#12-disable-or-limit-navigation
Electron.app.on('web-contents-created', (event, contents) => {
    contents.on('will-navigate', (event, navigationUrl) => {
        const parsedUrl = new URL(navigationUrl)
        if (parsedUrl.origin !== origin && !trustedHosts.includes(parsedUrl.host)) {
            event.preventDefault()
            console.error(`Prevented navigation to '${navigationUrl}'.`)
        }
    })
})

// === Disable New Windows Creation ===

/// Much like navigation, the creation of new webContents is a common attack vector. Attackers
/// attempt to convince your app to create new windows, frames, or other renderer processes with
/// more privileges than they had before or with pages opened that they couldn't open before.
/// Follow the link to learn more:
/// https://www.electronjs.org/docs/tutorial/security#13-disable-or-limit-creation-of-new-windows
Electron.app.on('web-contents-created', (event, contents) => {
    contents.on('new-window', async (event, navigationUrl) => {
        event.preventDefault()
        console.error(`Blocking new window creation request to '${navigationUrl}'`)
    })
})

// =======================
// === Project Manager ===
// =======================

function projectManagerPath() {
    let binPath = args['backend-path'] ?? projectManagerExecutable
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

function runBackend() {
    if (args.backend !== false) {
        let opts = args['--'] ? args['--'] : []
        if (args.verbose === true) {
            opts.push('-vv')
        }
        console.log('Starting the backend process with the following options:', opts)
        return spawnProjectManager(opts)
    }
}

async function backendVersion() {
    if (args.backend !== false) {
        return await execProjectManager(['--version']).then(t => t.stdout)
    }
}

// ============
// === Main ===
// ============

let hideInsteadOfQuit = false

let server = null
let mainWindow: null | Electron.BrowserWindow = null
let origin: null | string = null

async function main() {
    // Note [Main error handling]
    try {
        runBackend()

        console.log('Starting the IDE service.')
        if (args.server !== false) {
            let serverCfg = Object.assign({}, args)
            serverCfg.dir = root
            serverCfg.fallback = '/assets/index.html'
            server = await Server.create(serverCfg)
            origin = `http://localhost:${server.port}`
        }
        if (args.window !== false) {
            console.log('Starting the IDE client.')
            mainWindow = createWindow()
            mainWindow.on('close', evt => {
                if (hideInsteadOfQuit) {
                    evt.preventDefault()
                    mainWindow.hide()
                }
            })
        }
    } catch (err) {
        // Note [Main error handling]
        console.error('Failed to initialize IDE. Error:', err)
        Electron.app.quit()
    }
}

// Note [Main error handling]
// ==========================
// It is critical that the main function runs in its entirety. Otherwise, IDE enters a "zombie
// process" state, where Electron processes have been spawned, but there is no window and the user
// can't observe anything. Usually they will try to spawn another instance of the IDE, but this can
// fail because of these zombie process presence.
//
// The solution is to catch all errors and exit the process if any part of the initial setup fails.
// If it succeeds, at least the window will be shown, allowing the user to observe the error and
// close it.

function urlParamsFromObject(obj: { [key: string]: any }) {
    let params = []
    for (let key in obj) {
        let val = obj[key]
        params.push(`${key}=${val}`)
    }
    return params.join('&')
}

function createWindow() {
    let webPreferences = secureWebPreferences()
    webPreferences.preload = path.join(root, 'preload.cjs')
    webPreferences.sandbox = true
    webPreferences.backgroundThrottling = false

    let windowPreferences: Electron.BrowserWindowConstructorOptions = {
        webPreferences: webPreferences,
        width: windowSize.width,
        height: windowSize.height,
        frame: args.frame,
        transparent: false,
        titleBarStyle: 'default',
    }

    if (args.dev) {
        windowPreferences.webPreferences.devTools = true
    }

    if (args.frame === false && process.platform === 'darwin') {
        windowPreferences.titleBarStyle = 'hiddenInset'
    }

    if (args['background-throttling']) {
        windowPreferences.webPreferences.backgroundThrottling = true
    }

    if (args.vibrancy === true) {
        windowPreferences.vibrancy = 'fullscreen-ui'
    }

    remoteMain.initialize()
    const window = new Electron.BrowserWindow(windowPreferences)
    remoteMain.enable(window.webContents)
    window.setMenuBarVisibility(false)

    if (args.dev) {
        window.webContents.openDevTools()
    }

    let urlCfg = {
        platform: process.platform,
        frame: args.frame,
        theme: args.theme,
        darkTheme: Electron.nativeTheme.shouldUseDarkColors,
        // high_contrast: Electron.nativeTheme.shouldUseHighContrastColors,
        // crash_report_host: args.crashReportHost,
        dataGathering: args.dataGathering,
        preferredEngineVersion: args.preferredEngineVersion,
        enableNewComponentBrowser: args.enableNewComponentBrowser,
        emitUserTimingMeasurements: args.emitUserTimingMeasurements,
        nodeLabels: args.nodeLabels,
        debug: args.verbose,
        skip_min_version_check: false,
    }

    Electron.ipcMain.on('error', (event, data) => console.error(data))

    // We want to pass this argument only if explicitly passed. Otherwise we allow contents to select default behavior.
    if (typeof args.skipMinVersionCheck !== 'undefined') {
        urlCfg.skip_min_version_check = args.skipMinVersionCheck
    }
    if (args.project) {
        // @ts-ignore
        urlCfg.project = args.project
    }
    if (args.entryPoint) {
        // @ts-ignore
        urlCfg.entry = args.entryPoint
    }
    let profilePromises = []
    if (args.loadProfile) {
        profilePromises = args.loadProfile.map((path: string) => fsp.readFile(path, 'utf8'))
    }
    const profiles = Promise.all(profilePromises)
    Electron.ipcMain.on('load-profiles', event => {
        profiles.then(profiles => {
            event.reply('profiles-loaded', profiles)
        })
    })
    if (args.saveProfile) {
        Electron.ipcMain.on('save-profile', (event, data) => {
            fss.writeFileSync(args.saveProfile, data)
        })
    }
    if (args.workflow) {
        // @ts-ignore
        urlCfg.testWorkflow = args.workflow
    }

    Electron.ipcMain.on('quit-ide', () => {
        Electron.app.quit()
    })

    let params = urlParamsFromObject(urlCfg)
    let address = `${origin}?${params}`

    console.log(`Loading the window address ${address}`)
    window.loadURL(address)
    return window
}

/// By default, Electron will automatically approve all permission requests unless the developer has
/// manually configured a custom handler. While a solid default, security-conscious developers might
/// want to assume the very opposite. Follow the link to learn more:
// https://www.electronjs.org/docs/tutorial/security#4-handle-session-permission-requests-from-remote-content
function setupPermissions() {
    Electron.session.defaultSession.setPermissionRequestHandler(
        (webContents, permission, callback) => {
            const url = webContents.getURL()
            console.error(`Unhandled permission request '${permission}'.`)
        }
    )
}

// ==============
// === Events ===
// ==============

Electron.app.on('activate', () => {
    if (process.platform === 'darwin') {
        mainWindow.show()
    }
})

Electron.app.on('ready', () => {
    if (args.version) {
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
    } else if (args.info) {
        printDebugInfo()
    } else {
        main()
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
            Electron.BrowserWindow.getFocusedWindow().webContents.toggleDevTools()
        }
        if (control && alt && shift && !meta && code === 'KeyR') {
            Electron.BrowserWindow.getFocusedWindow().reload()
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
