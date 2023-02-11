'use strict'

import assert from 'node:assert'
import buildCfg from '../../../build.json'
import Electron from 'electron'
import isDev from 'electron-is-dev'
import path from 'node:path'
import * as server from 'bin/server'
import * as content from '../../content/src/config'
import * as security from './security'
import * as naming from './naming'
import * as config from './config'
import * as paths from './paths'

import * as argParser from './arg-parser'
import * as debug from './debug'
import * as projectManager from 'bin/project-manager'

const logger = content.logger

// ============
// === Help ===
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
    args: config.Args
    windowSize: config.WindowSize
    // FIXME any
    backendOptions: any
    constructor() {
        const { args, windowSize, backendOptions } = argParser.parseArgs()
        this.args = args
        this.windowSize = windowSize
        this.backendOptions = backendOptions
        security.enableAll()
        Electron.app.whenReady().then(() => {
            if (args.options.version.value) {
                printVersion(this.args)
            } else if (args.options.info.value) {
                debug.printDebugInfo()
            } else {
                this.main()
            }
        })
    }

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
        if (!this.args.groups.engine.options.backend.value) {
            console.log('The app is configured not to run the backend process.')
        } else {
            const dashArgsOpt = this.backendOptions
            const dashArgs = dashArgsOpt ? dashArgsOpt : []
            const verboseArgs = this.args.groups.debug.options.verbose.value ? ['-vv'] : []
            const args2 = dashArgs.concat(verboseArgs)
            return projectManager.spawn(this.args, args2)
        }
    }

    async startContentServer() {
        if (!this.args.options.server.value) {
            logger.log('The app is configured not to run the content server.')
        } else {
            await logger.asyncGroupMeasured('Starting the content server.', async () => {
                let serverCfg = new server.Config({
                    dir: paths.app,
                    port: this.args.groups.server.options.port.value,
                    fallback: '/assets/index.html',
                })
                xserver = await server.Server.create(serverCfg)
                origin = `http://localhost:${xserver.config.port}`
            })
        }
    }

    createWindow() {
        if (!this.args.options.window.value) {
            logger.log('The app is configured not to create a window.')
        } else {
            logger.groupMeasured('Creating the window.', () => {
                const webPreferences = security.secureWebPreferences()
                webPreferences.preload = path.join(paths.app, 'preload.cjs')
                webPreferences.sandbox = true
                webPreferences.backgroundThrottling =
                    this.args.groups.performance.options.backgroundThrottling.value
                webPreferences.devTools = this.args.groups.debug.options.dev.value

                let windowPreferences: Electron.BrowserWindowConstructorOptions = {
                    webPreferences,
                    width: this.windowSize.width,
                    height: this.windowSize.height,
                    frame: this.args.groups.window.options.frame.value,
                    transparent: false,
                    titleBarStyle: 'default',
                }

                if (windowPreferences.frame === false && process.platform === 'darwin') {
                    windowPreferences.titleBarStyle = 'hiddenInset'
                }

                if (this.args.groups.window.options.vibrancy.value) {
                    windowPreferences.vibrancy = 'fullscreen-ui'
                }

                const window = new Electron.BrowserWindow(windowPreferences)
                window.setMenuBarVisibility(false)

                if (this.args.groups.debug.options.dev.value) {
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
            for (const option of this.args.optionsRecursive()) {
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

function printVersion(args: config.Args) {
    let indent = ' '.repeat(4)
    let maxNameLen = 0
    for (let name in debug.versionInfo) {
        if (name.length > maxNameLen) {
            maxNameLen = name.length
        }
    }

    console.log('Frontend:')
    for (let name in debug.versionInfo) {
        let label = naming.capitalizeFirstLetter(name)
        let spacing = ' '.repeat(maxNameLen - name.length)
        // @ts-ignore
        console.log(`${indent}${label}:${spacing} ${versionInfo[name]}`)
    }

    console.log('')
    console.log('Backend:')
    projectManager.version(args).then(backend => {
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
