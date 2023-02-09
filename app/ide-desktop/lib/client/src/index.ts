'use strict'

import assert from 'node:assert'
import buildCfg from '../../../build.json'
import Electron from 'electron'
import isDev from 'electron-is-dev'
import path from 'node:path'
// @ts-ignore
import * as server from './server'
import util from 'node:util'
import { hideBin } from 'yargs/helpers'
import { project_manager_bundle } from '../paths.js'
import * as content from '../../content/src/config'
import chalk from 'chalk'
import * as security from './security'
import * as naming from './naming'
import * as config from './config'

import child_process, { SpawnOptions } from 'child_process'
import fss from 'node:fs'
import fsp from 'node:fs/promises'

import stringLength from 'string-length'

const logger = content.logger
const yargs = require('yargs')

const execFile = util.promisify(child_process.execFile)

const options = config.options

// =============
// === Paths ===
// =============

/** File system paths used by the application. */
class Paths {
    static app = Electron.app.getAppPath()
    static resources = path.join(Paths.app, '..')
    static projectManager = path.join(
        Paths.resources,
        project_manager_bundle,
        // @ts-ignore
        // Placeholder for a bundler-provided define.
        PROJECT_MANAGER_IN_BUNDLE_PATH
    )
}

// ============
// === Help ===
// ============

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
    config: typeof config.options
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
            const cmdOption = naming.camelToKebabCase(option.qualifiedName())
            maxOptionLength = Math.max(maxOptionLength, stringLength(cmdOption))
            const entry = [cmdOption, option]
            section.entries.push(entry)
        }
    }

    for (const [optionName, option] of Object.entries(cfg.config.options)) {
        const cmdOption = naming.camelToKebabCase(optionName)
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
        const groupTitle = chalk.bold(`${naming.camelCaseToTitle(groupName)} Options `)
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
    opts[naming.camelToKebabCase(option.qualifiedName())] = yargsParam
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

for (const option of options.optionsRecursive()) {
    const arg = xargs[naming.camelToKebabCase(option.qualifiedName())]
    if (arg != null) {
        option.value = arg
        option.setByUser = true
    }
}

let windowSize = config.WindowSize.default()
const parsedWindowSize = config.WindowSize.parse(options.groups.window.options.size.value)

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
    let binPath = options.groups.engine.options.projectManagerPath.value || Paths.projectManager
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
    return logger.groupMeasured(
        `Starting the backend process with the following options: ${args}`,
        () => {
            let binPath = projectManagerPath()
            let stdin: 'pipe' = 'pipe'
            let stdout: 'inherit' = 'inherit'
            let stderr: 'inherit' = 'inherit'
            let opts: SpawnOptions = {
                stdio: [stdin, stdout, stderr],
            }
            let out = child_process.spawn(binPath, args, opts)
            logger.log(`Project Manager has been spawned (pid = ${out.pid}).`)
            out.on('exit', code => {
                logger.log(`Project Manager exited with code ${code}.`)
            })
            return out
        }
    )
}

async function backendVersion() {
    if (options.groups.engine.options.backend.value) {
        return await execProjectManager(['--version']).then(t => t.stdout)
    }
}

// ============
// === Main ===
// ============

let hideInsteadOfQuit = false

let xserver = null
let mainWindow: null | Electron.BrowserWindow = null
let origin: null | string = null

function urlParamsFromObject(obj: { [key: string]: string }) {
    let params = []
    for (let key in obj) {
        let val = obj[key]
        params.push(`${key}=${val}`)
    }
    return params.length == 0 ? '' : '?' + params.join('&')
}

class App {
    window: null | Electron.BrowserWindow = null
    async main() {
        // We catch all errors here. Otherwise, it might be possible that the app will run partially
        // and the user will not see anything.
        try {
            await logger.asyncGroupMeasured('Starting the application', async () => {
                // Note that we want to do all the actions synchronously, so when the window
                // appears, it serves the website immediately.
                this.startBackend()
                await this.startContentServer()
                this.createWindow()
                this.loadWindowContent()
            })
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
            logger.log('The app is configured not to run the content server.')
        } else {
            await logger.asyncGroupMeasured('Starting the content server.', async () => {
                let serverCfg = new server.Config({
                    dir: Paths.app,
                    port: options.groups.server.options.port.value,
                    fallback: '/assets/index.html',
                })
                xserver = await server.Server.create(serverCfg)
                origin = `http://localhost:${xserver.config.port}`
            })
        }
    }

    createWindow() {
        if (!options.options.window.value) {
            logger.log('The app is configured not to create a window.')
        } else {
            logger.groupMeasured('Creating the window.', () => {
                const webPreferences = security.secureWebPreferences()
                webPreferences.preload = path.join(Paths.app, 'preload.cjs')
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

                this.window = window
            })
        }
    }

    loadWindowContent() {
        const window = this.window
        if (window != null) {
            const urlCfg: { [key: string]: string } = {}
            for (const option of options.optionsRecursive()) {
                if (option.setByUser) {
                    urlCfg[option.qualifiedName()] = String(option.value)
                }
            }
            let params = urlParamsFromObject(urlCfg)
            let address = `${origin}${params}`

            logger.log(`Loading the window address ${address}`)
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
        let label = naming.capitalizeFirstLetter(name)
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
security.enableAll()
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
