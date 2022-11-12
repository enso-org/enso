'use strict'

import { defaultLogServerHost } from '../../../config.js'
import assert from 'node:assert'
import buildCfg from '../../../build.json'
import Electron from 'electron'
import isDev from 'electron-is-dev'
import path from 'node:path'
import * as Server from './server.js'
import util from 'node:util'
import yargs from 'yargs/yargs'
import remoteMain from '@electron/remote/main/index.js'

import { project_manager_bundle } from '../paths.js'

import child_process from 'child_process'
import fss from 'node:fs'
import fsp from 'node:fs/promises'

// =============
// === Paths ===
// =============

const root = Electron.app.getAppPath()
const resources = path.join(root, '..')
const project_manager_executable = path.join(
    resources,
    project_manager_bundle,
    PROJECT_MANAGER_IN_BUNDLE_PATH // Placeholder for a bundler-provided define.
)

// ================
// === Defaults ===
// ================

let windowCfg = {
    width: 1380,
    height: 900,
}

// =============
// === Utils ===
// =============

function capitalizeFirstLetter(string) {
    return string.charAt(0).toUpperCase() + string.slice(1)
}

const execFile = util.promisify(child_process.execFile)

// The list of hosts that the app can access. They are required for
// user authentication to work.
const trustedHosts = [
    'enso-org.firebaseapp.com',
    'accounts.google.com',
    'accounts.youtube.com',
    'github.com',
]

// =====================
// === Option Parser ===
// =====================

let usage = `
${buildCfg.name} ${buildCfg.version} command line interface.

Usage: enso [options] [--electron electron-args...] [-- backend-args...]

Arguments that follow the two dashes (\`--\`) will be passed to the backend process. They are used\
 if IDE spawns backend, i.e. if '--backend false' has not been set.
`

let electronOptsUsage = `Electron Options:

The following options should be passed after the \`--electron\` flag but before the \`--\` flag.\
 Alternatively, they can be prefixed with \`electron-\` and used as regular options. For example,\
 to restrict disk cache size usage, you can use either \`--electron --disk-cache-size 1024\` or\
 \`--electron-disk-cache-size 1024\`.`

let optParser = yargs()
    .scriptName('')
    .usage(usage)
    .help(false)
    .version(false)
    // Makes all flags passed after '--' be one string.
    .parserConfiguration({ 'populate--': true })
    .strict()

let electronOptParser = yargs()
    .scriptName('')
    .usage(electronOptsUsage)
    .help(false)
    .version(false)
    // Makes all flags passed after '--' be one string.
    .parserConfiguration({ 'populate--': true })
    .strict()

// === Config Options ===

let configOptionsGroup = 'Config Options:'

optParser.options('port', {
    group: configOptionsGroup,
    describe: `Port to use. [${Server.DEFAULT_PORT}]`,
})

optParser.options('project', {
    group: configOptionsGroup,
    describe: 'Open the specified project on startup.',
})

optParser.options('server', {
    group: configOptionsGroup,
    describe: 'Run the server. [true]',
    type: 'boolean',
})

optParser.options('window', {
    group: configOptionsGroup,
    describe: 'Show the window. [true]',
    type: 'boolean',
})

optParser.options('background-throttling', {
    group: configOptionsGroup,
    describe: 'Throttle animations when run in background. [false]',
    type: 'boolean',
})

optParser.options('backend', {
    group: configOptionsGroup,
    describe: 'Start the backend process automatically. [true]',
    type: 'boolean',
})

optParser.options('backend-path', {
    group: configOptionsGroup,
    describe: 'Set the path of a local project manager to use for running projects.',
})

optParser.options('crash-report-host', {
    group: configOptionsGroup,
    describe:
        'The address of the server that will receive crash reports. ' +
        'Consists of a hostname, optionally followed by a ":" and a port number.',
    requiresArg: true,
    default: defaultLogServerHost,
})

optParser.options('data-gathering', {
    group: configOptionsGroup,
    describe: 'Enable the sharing of any usage data.',
    type: 'boolean',
    default: true,
})

optParser.options('preferred-engine-version', {
    group: configOptionsGroup,
    describe: 'The Engine version that IDE will try to use for newly created projects.',
    type: 'string',
    default: BUNDLED_ENGINE_VERSION,
})

optParser.options('enable-new-component-browser', {
    group: configOptionsGroup,
    describe:
        'Enable to have new Component Browser panel in place of old Node Searcher. A temporary ' +
        'feature flag, until the Component Browser is stable.',
    type: 'boolean',
    default: true,
})

optParser.options('skip-min-version-check', {
    group: configOptionsGroup,
    describe: 'Disables the check whether this IDE version is still supported.',
    type: 'boolean',
})

// === Debug Options ===

let debugOptionsGroup = 'Debug Options:'

optParser.options('verbose', {
    group: debugOptionsGroup,
    describe: `Increase logs verbosity. Affects both IDE and the backend.`,
    default: false,
    type: `boolean`,
})

optParser.options('entry-point', {
    group: debugOptionsGroup,
    describe:
        'Run an alternative entry point (e.g. one of the debug scenes). To see list of ' +
        'entry points, do not provide the argument.',
})

optParser.options('dev', {
    group: debugOptionsGroup,
    describe: 'Run the application in development mode.',
})

optParser.options('devtron', {
    group: debugOptionsGroup,
    describe: 'Install the Devtron Developer Tools extension.',
})

optParser.options('load-profile', {
    group: debugOptionsGroup,
    describe:
        'Load a performance profile. For use with developer tools such as the ' +
        "'profiling-run-graph' entry point.",
    requiresArg: true,
    type: `array`,
})

optParser.options('save-profile', {
    group: debugOptionsGroup,
    describe: 'Record a performance profile and write to a file.',
    requiresArg: true,
    type: `string`,
})

optParser.options('workflow', {
    group: debugOptionsGroup,
    describe: 'Specify a workflow for profiling. Must be used with --entry-point=profile.',
    requiresArg: true,
    type: `string`,
})

// === Style Options ===

let styleOptionsGroup = 'Style Options:'

optParser.options('frame', {
    group: styleOptionsGroup,
    describe: 'Draw window frame. [false] on MacOS, [true] otherwise.',
    type: `boolean`,
})

optParser.options('vibrancy', {
    group: styleOptionsGroup,
    describe: 'Use the vibrancy effect.',
    default: false,
    type: `boolean`,
})

optParser.options('window-size', {
    group: styleOptionsGroup,
    describe: `Set the window size. [${windowCfg.width}x${windowCfg.height}]`,
    requiresArg: true,
})

optParser.options('theme', {
    group: styleOptionsGroup,
    describe: 'Use the provided theme. [light]',
    type: `string`,
})

optParser.options('node-labels', {
    group: styleOptionsGroup,
    describe: 'Show node labels. [true]',
    default: true,
    type: `boolean`,
})

// === Other Options ===

let infoOptionsGroup = 'Debug Information:'

optParser.options('help', {
    group: infoOptionsGroup,
    describe: `Print the help information.`,
})

const HELP_EXTRA_KEY = 'help-extra'
optParser.options(HELP_EXTRA_KEY, {
    group: infoOptionsGroup,
    describe: `Print the help information including less common options.`,
})

optParser.options('info', {
    group: infoOptionsGroup,
    describe: `Print the system debug info.`,
})

optParser.options('version', {
    group: infoOptionsGroup,
    describe: `Print the version.`,
})

// === Electrion Options ===

let electronOptions = {
    'auth-server-whitelist': {
        describe:
            'A comma-separated list of servers for which integrated authentication is ' +
            'enabled.',
        nargs: 1,
        type: 'string',
        requiresArg: true,
    },
    'auth-negotiate-delegate-whitelist': {
        describe:
            'A comma-separated list of servers for which delegation of user credentials is ' +
            "required. Without '*' prefix the URL has to match exactly.",
        nargs: 1,
        type: 'string',
        requiresArg: true,
    },
    'disable-ntlm-v2': {
        describe: 'Disables NTLM v2 for posix platforms, no effect elsewhere.',
        nargs: 0,
    },
    'disable-http-cache': {
        describe: 'Disables the disk cache for HTTP requests.',
        nargs: 0,
    },
    'disable-http2': {
        describe: 'Disable HTTP/2 and SPDY/3.1 protocols.',
        nargs: 0,
    },
    'disable-renderer-backgrounding': {
        describe:
            "Prevents Chromium from lowering the priority of invisible pages' renderer " +
            'processes.',
        nargs: 0,
    },
    'disk-cache-size': {
        describe: 'Forces the maximum disk space to be used by the disk cache, in bytes.',
        nargs: 1,
        type: 'number',
        requiresArg: true,
    },
    'enable-logging': {
        describe: "Prints Chromium's logging to stderr (or a log file, if provided as argument).",
        type: 'string',
    },
    'force-fieldtrials': {
        describe:
            'Field trials to be forcefully enabled or disabled. For example, ' +
            "'WebRTC-Audio-Red-For-Opus/Enabled/'.",
        nargs: 1,
        type: 'string',
        requiresArg: true,
    },
    'host-rules': {
        describe:
            'A comma-separated list of rules that control how hostnames are mapped. For ' +
            "example, 'MAP * 127.0.0.1'.",
        nargs: 1,
        type: 'string',
        requiresArg: true,
    },
    'host-resolver-rules': {
        describe: "Like '--host-rules' but these rules only apply to the host resolver.",
        nargs: 1,
        type: 'string',
        requiresArg: true,
    },
    'ignore-certificate-errors': {
        describe: "Ignore the connections limit for domains list separated by ','.",
        nargs: 0,
    },
    'ignore-connections-limit': {
        describe: "Ignore the connections limit for domains list separated by ','.",
        nargs: 1,
        type: 'string',
        requiresArg: true,
    },
    'js-flags': {
        describe:
            'Specifies the flags passed to the Node.js engine. For example, ' +
            '\'--js-flags="--harmony_proxies --harmony_collections"\'.',
        nargs: 1,
        type: 'string',
        requiresArg: true,
    },
    lang: {
        describe: 'Set a custom locale.',
        nargs: 1,
        type: 'string',
        requiresArg: true,
    },
    'log-file': {
        describe:
            "If '--enable-logging' is specified, logs will be written to the given path. " +
            'The parent directory must exist.',
        nargs: 1,
        type: 'string',
        requiresArg: true,
    },
    'log-net-log': {
        describe: 'Enables net log events to be saved and writes them to the provided path.',
        nargs: 1,
        type: 'string',
        requiresArg: true,
    },
    'log-level': {
        describe:
            "Sets the verbosity of logging when used together with '--enable-logging'. The " +
            "argument should be one of Chrome's LogSeverities.",
        nargs: 1,
        type: 'string',
        requiresArg: true,
    },
    'no-proxy-server': {
        describe: "Ignore the connections limit for domains list separated by ','.",
        nargs: 0,
    },
    'no-sandbox': {
        describe:
            'Disables the Chromium sandbox. Forces renderer process and Chromium helper ' +
            'processes to run un-sandboxed. Should only be used for testing.',
        nargs: 0,
    },
    'proxy-bypass-list': {
        describe:
            'Instructs Electron to bypass the proxy server for the given ' +
            'semi-colon-separated list of hosts. This flag has an effect only if used in tandem ' +
            "with '--proxy-server'. For example, " +
            '\'--proxy-bypass-list "<local>;*.google.com;*foo.com;1.2.3.4:5678"\'.',
        nargs: 1,
        type: 'string',
        requiresArg: true,
    },
    'proxy-pac-url': {
        describe: 'Uses the PAC script at the specified url.',
        nargs: 1,
        type: 'string',
        requiresArg: true,
    },
    'proxy-server': {
        describe:
            "Use a specified proxy server ('address:port'), which overrides the system " +
            'setting. This switch only affects requests with HTTP protocol, including HTTPS and ' +
            'WebSocket requests. It is also noteworthy that not all proxy servers support HTTPS ' +
            'and WebSocket requests. The proxy URL does not support username and password ' +
            'authentication per ' +
            '[Chromium issue](https://bugs.chromium.org/p/chromium/issues/detail?id=615947).',
        nargs: 1,
        type: 'string',
        requiresArg: true,
    },
    'remote-debugging-port': {
        describe: 'Enables remote debugging over HTTP on the specified port.',
        nargs: 1,
        type: 'string',
        requiresArg: true,
    },
    v: {
        describe:
            'Gives the default maximal active V-logging level; 0 is the default. Normally ' +
            'positive values are used for V-logging levels. This switch only works when ' +
            "'--enable-logging' is also passed.",
        nargs: 1,
        type: 'number',
        requiresArg: true,
    },
    vmodule: {
        describe:
            'Gives the per-module maximal V-logging levels to override the value given by ' +
            "'--v'. E.g. 'my_module=2,foo*=3' would change the logging level for all code in " +
            "source files 'my_module.*' and 'foo*.*'. Any pattern containing a forward or " +
            'backward slash will be tested against the whole pathname and not only the module. ' +
            "This switch only works when '--enable-logging' is also passed.",
        nargs: 1,
        type: 'string',
        requiresArg: true,
    },
    force_high_performance_gpu: {
        describe: 'Force using discrete GPU when there are multiple GPUs available.',
        nargs: 0,
    },
    force_low_power_gpu: {
        describe: 'Force using integrated GPU when there are multiple GPUs available.',
        nargs: 0,
    },
}

for (let option in electronOptions) {
    optParser.options(
        `electron-${option}`,
        Object.assign({ hidden: true }, electronOptions[option])
    )
    electronOptParser.options(option, Object.assign({ group: ' ' }, electronOptions[option]))
}

// === Parsing ===

const ELECTRON_FLAG_SEPARATOR = '--electron'
const BACKEND_FLAG_SEPARATOR = '--'

async function parseCmdArgs() {
    console.log('isDev', isDev)
    let argv = isDev ? process.argv.slice(process.argv.indexOf('--') + 1) : process.argv

    // === Extracting electron flags (between the `--electron` and the `--` flag) ===

    let hasElectronFlags = argv.includes(ELECTRON_FLAG_SEPARATOR)
    let hasBackendFlags = argv.includes(BACKEND_FLAG_SEPARATOR)

    let backendFlags = hasBackendFlags ? argv.slice(argv.indexOf(BACKEND_FLAG_SEPARATOR)) : []
    let rest = hasBackendFlags ? argv.slice(0, argv.indexOf(BACKEND_FLAG_SEPARATOR)) : argv
    let electronFlags = hasElectronFlags
        ? rest.slice(rest.indexOf(ELECTRON_FLAG_SEPARATOR) + 1)
        : []
    let appFlags = hasElectronFlags ? rest.slice(0, rest.indexOf(ELECTRON_FLAG_SEPARATOR)) : rest

    // === Parsing arguments ===

    let unknownArgsRegexp = new RegExp(String.raw`(\n)?Unknown argument(s)?:.*`, 'gm')

    let optParserErr
    let optParserResult
    let optParserArgv = appFlags.concat(backendFlags)
    optParser.parse(optParserArgv, (err, result) => {
        optParserErr = err ? err.message.replace(unknownArgsRegexp, '') : ''
        optParserResult = result
    })

    let electronOptParserErr
    let electronOptParserResult
    electronOptParser.parse(electronFlags, (err, result) => {
        electronOptParserErr = err ? err.message.replace(unknownArgsRegexp, '') : ''
        electronOptParserResult = result
    })

    let result = Object.assign(electronOptParserResult, optParserResult)
    let optParserHelp = await optParser.getHelp()
    let electronOptParserHelp = await electronOptParser.getHelp()

    // === Gluing errors from both arg parsers ===

    let error = null
    if (optParserErr || electronOptParserErr) {
        error = `Unknown arguments: `
        if (optParserErr) {
            error += optParserErr
        }
        if (electronOptParserErr) {
            if (optParserErr) {
                error += ', '
            }
            error += electronOptParserErr
        }
    }

    // === Printing errors and help if requested by the user ===

    if (error || result.help || result[HELP_EXTRA_KEY]) {
        console.log(optParserHelp)
        let electronHelp =
            result[HELP_EXTRA_KEY] || electronOptParserErr || optParserErr.includes('electron')
        if (electronHelp) {
            console.log() // newline
            console.log(electronOptParserHelp)
        }
        if (error) {
            console.log() // newline
            console.log(error)
        }
        if (!electronHelp) {
            console.log() // newline
            console.log(`Use '--${HELP_EXTRA_KEY}' to see less common options.`)
        }
        if (error) {
            process.exit(1)
        } else {
            process.exit(0)
        }
    }

    return result
}

// Top-level await is not supported. This is a hack:
// https://usefulangle.com/post/248/javascript-async-anonymous-function-iife
let args = (async () => {
    await parseCmdArgs()
})()

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
        windowCfg.width = width
        windowCfg.height = height
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
function secureWebPreferences(webPreferences) {
    if (!webPreferences) {
        webPreferences = {}
    }
    delete webPreferences.preload
    delete webPreferences.preloadURL
    delete webPreferences.nodeIntegration
    delete webPreferences.nodeIntegrationInWorker
    delete webPreferences.webSecurity
    delete webPreferences.allowRunningInsecureContent
    delete webPreferences.experimentalFeatures
    delete webPreferences.enableBlinkFeatures
    delete webPreferences.allowpopups
    delete webPreferences.contextIsolation
    return webPreferences
}

let urlWhitelist = []
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
    let binPath = args['backend-path'] ?? project_manager_executable
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
async function execProjectManager(args) {
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
function spawnProjectManager(args) {
    let binPath = projectManagerPath()
    let stdin = 'pipe'
    let stdout = 'inherit'
    let stderr = 'inherit'
    let opts = {
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
let mainWindow = null
let origin = null

async function main(args) {
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
        console.error('Failed to setup IDE. Error:', err)
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

function urlParamsFromObject(obj) {
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

    let windowPreferences = {
        webPreferences: webPreferences,
        width: windowCfg.width,
        height: windowCfg.height,
        frame: args.frame,
        devTools: false,
        sandbox: true,
        backgroundThrottling: false,
        transparent: false,
        titleBarStyle: 'default',
    }

    if (args.dev) {
        windowPreferences.devTools = true
    }

    if (args.frame === false && process.platform === 'darwin') {
        windowPreferences.titleBarStyle = 'hiddenInset'
    }

    if (args['background-throttling']) {
        windowPreferences.backgroundThrottling = true
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
        dark_theme: Electron.nativeTheme.shouldUseDarkColors,
        high_contrast: Electron.nativeTheme.shouldUseHighContrastColors,
        crash_report_host: args.crashReportHost,
        data_gathering: args.dataGathering,
        preferred_engine_version: args.preferredEngineVersion,
        enable_new_component_browser: args.enableNewComponentBrowser,
        node_labels: args.nodeLabels,
        verbose: args.verbose,
    }

    Electron.ipcMain.on('error', (event, data) => console.error(data))

    // We want to pass this argument only if explicitly passed. Otherwise we allow contents to select default behavior.
    if (typeof args.skipMinVersionCheck !== 'undefined') {
        urlCfg.skip_min_version_check = args.skipMinVersionCheck
    }
    if (args.project) {
        urlCfg.project = args.project
    }
    if (args.entryPoint) {
        urlCfg.entry = args.entryPoint
    }
    let profilePromises = []
    if (args.loadProfile) {
        profilePromises = args.loadProfile.map(path => fsp.readFile(path, 'utf8'))
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
        urlCfg.test_workflow = args.workflow
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
        main(args)
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
