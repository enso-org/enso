'use strict'

import * as buildCfg  from '../../../../../dist/build.json'
import * as Electron  from 'electron'
import * as isDev     from 'electron-is-dev'
import * as minimist  from 'minimist'
import * as path      from 'path'
import * as pkg       from '../package.json'
import * as rootCfg   from '../../../package.json'
import * as Server    from 'enso-studio-common/src/server'
import * as yargs     from 'yargs'


// FIXME default options parsed wrong
// https://github.com/yargs/yargs/issues/1590

// ================
// === Defaults ===
// ================

let windowCfg = {
    width  : 640,
    height : 640,
}



// =====================
// === Option Parser ===
// =====================

let usage = `
${pkg.build.productName} ${rootCfg.version} command line interface.

Usage: ${pkg.build.productName} [options]
`

let optParser = yargs
    .scriptName("")
    .usage(usage)
    .help()
    .version(false)
    .parserConfiguration({'populate--':true})
    .strict()


// === Config Options ===

let configOptionsGroup = 'Config Options:'

optParser.options('port', {
    group    : configOptionsGroup,
    describe : `Port to use [${Server.DEFAULT_PORT}]`,
})

optParser.options('project', {
    group    : configOptionsGroup,
    describe : 'Open the specified project on startup',
})

optParser.options('server', {
    group    : configOptionsGroup,
    describe : 'Run the server [true]',
})

optParser.options('window', {
    group    : configOptionsGroup,
    describe : 'Show the window [true]',
})

optParser.options('background-throttling', {
    group    : configOptionsGroup,
    describe : 'Throttle animations when run in background [false]',
})


// === Debug Options ===

let debugOptionsGroup = 'Debug Options:'

optParser.options('debug-scene', {
    group       : debugOptionsGroup,
    describe    : 'Run the debug scene instead of the main app',
    requiresArg : true
})

optParser.options('dev', {
    group       : debugOptionsGroup,
    describe    : 'Run the application in development mode',
})

optParser.options('devtron', {
    group       : debugOptionsGroup,
    describe    : 'Install the Devtron Developer Tools extension',
})


// === Style Options ===

let styleOptionsGroup = 'Style Options:'

optParser.options('frame', {
    group       : styleOptionsGroup,
    describe    : 'Draw window frame',
    default     : true,
    type        : `boolean`
})

optParser.options('vibrancy', {
    group       : styleOptionsGroup,
    describe    : 'Use the vibrancy effect',
    default     : false,
    type        : `boolean`
})

optParser.options('window-size', {
    group       : styleOptionsGroup,
    describe    : `Set the window size [${windowCfg.width}x${windowCfg.height}]`,
    requiresArg : true
})


// === Other Options ===

optParser.options('info', {
    describe    : `Print the system debug info`,
})

optParser.options('version', {
    describe    : `Print the version`,
})


// === Parsing ===

function parseCmdArgs() {
    let argv = isDev ? process.argv.slice(process.argv.indexOf('--') + 1) : process.argv
    return optParser.parse(argv)
}

let args = parseCmdArgs()

if (args.windowSize) {
    let size   = args.windowSize.split('x')
    let width  = parseInt(size[0])
    let height = parseInt(size[1])
    if (isNaN(width) || isNaN(height)) {
        console.error(`Incorrect window size provided '${args.windowSize}'.`)
    } else {
        windowCfg.width  = width
        windowCfg.height = height
    }
}



// ==================
// === Debug Info ===
// ==================

let versionInfo = {
    core: rootCfg.version,
    build: buildCfg.buildVersion,
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
    console.log(JSON.stringify(info,undefined,4))
    process.exit();
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
    if(!webPreferences) { webPreferences = {} }
    delete webPreferences.preload
    delete webPreferences.preloadURL
    delete webPreferences.nodeIntegration
    delete webPreferences.nodeIntegrationInWorker
    delete webPreferences.webSecurity
    delete webPreferences.allowRunningInsecureContent
    delete webPreferences.experimentalFeatures
    delete webPreferences.enableBlinkFeatures
    delete webPreferences.allowpopups
    // TODO[WD]: We may want to enable it and use IPC to communicate with preload script.
    //           https://stackoverflow.com/questions/38335004/how-to-pass-parameters-from-main-process-to-render-processes-in-electron
    // webPreferences.contextIsolation = true
    // webPreferences.enableRemoteModule = false
    return webPreferences
}

let urlWhitelist = []
Electron.app.on('web-contents-created', (event,contents) => {
    contents.on('will-attach-webview', (event,webPreferences,params) => {
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
Electron.app.on('web-contents-created', (event,contents) => {
    contents.on('will-navigate', (event,navigationUrl) => {
        event.preventDefault()
        console.error(`Prevented navigation to '${navigationUrl}'`)
    })
})


// === Disable New Windows Creation ===

/// Much like navigation, the creation of new webContents is a common attack vector. Attackers
/// attempt to convince your app to create new windows, frames, or other renderer processes with
/// more privileges than they had before or with pages opened that they couldn't open before.
/// Follow the link to learn more:
/// https://www.electronjs.org/docs/tutorial/security#13-disable-or-limit-creation-of-new-windows
Electron.app.on('web-contents-created', (event,contents) => {
    contents.on('new-window', async (event,navigationUrl) => {
        event.preventDefault()
        console.error(`Blocking new window creation request to '${navigationUrl}'`)
    })
})



// ============
// === Main ===
// ============

let root = Electron.app.getAppPath()
let hideInsteadOfQuit = false

let server     = null
let mainWindow = null

async function main() {
    if(args.server !== false) {
        let serverCfg      = Object.assign({},args)
        serverCfg.dir      = root
        serverCfg.fallback = '/assets/index.html'
        server             = await Server.create(serverCfg)
    }
    mainWindow = createWindow()
    mainWindow.on("close", (evt) => {
       if (hideInsteadOfQuit) {
           evt.preventDefault()
           mainWindow.hide()
       }
   })
}

let port = Server.DEFAULT_PORT
if      (server)    { port = server.port }
else if (args.port) { port = args.port }

function urlParamsFromObject(obj) {
    let params = []
    for (let key in obj) {
        let val = obj[key]
        if      (val === false) {}
        else if (val === true)  { params.push(key) }
        else                    { params.push(`${key}=${val}`) }
    }
    return params.join("&")
}

function createWindow() {
    let webPreferences     = secureWebPreferences()
    webPreferences.preload = path.join(root,'preload.js')
    let windowPreferences  = {
        webPreferences       : webPreferences,
        width                : windowCfg.width,
        height               : windowCfg.height,
        frame                : true,
        devTools             : false,
        sandbox              : true,
        backgroundThrottling : false,
        transparent          : false,
        backgroundColor      : "#00000000",
        titleBarStyle        : 'default'
    }

    if (args.dev) {
        windowPreferences.devTools = true
    }

    if (args.frame === false) {
        windowPreferences.frame         = false
        windowPreferences.titleBarStyle = 'hiddenInset'
    }

    if (args['background-throttling']) {
        windowPreferences.backgroundThrottling = true
    }

    if (args.vibrancy === true) {
        windowPreferences.vibrancy = 'fullscreen-ui'
    }

    const window = new Electron.BrowserWindow(windowPreferences)

    window.setMenuBarVisibility(false)

    if (args.dev) {
        window.webContents.openDevTools()
    }

    let urlCfg = {
        desktop      : true,
        dark         : Electron.nativeTheme.shouldUseDarkColors,
        highContrast : Electron.nativeTheme.shouldUseHighContrastColors,
    }

    if (args.project) {
        urlCfg.project = args.project;
    }

    let params      = urlParamsFromObject(urlCfg)
    let targetScene = ""
    if (args.debugScene) {
        targetScene = `debug/${args.debugScene}`
    }
    let address = `http://localhost:${port}/${targetScene}?${params}`
    console.log(`Loading the window address ${address}`)
    window.loadURL(address)
    return window
}

/// By default, Electron will automatically approve all permission requests unless the developer has
/// manually configured a custom handler. While a solid default, security-conscious developers might
/// want to assume the very opposite. Follow the link to learn more:
// https://www.electronjs.org/docs/tutorial/security#4-handle-session-permission-requests-from-remote-content
function setupPermissions() {
    Electron.session.defaultSession.setPermissionRequestHandler (
        (webContents,permission,callback) => {
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
        console.log(`core     : ${versionInfo.core}`)
        console.log(`build    : ${versionInfo.build}`)
        console.log(`electron : ${versionInfo.electron}`)
        console.log(`chrome   : ${versionInfo.chrome}`)
        process.exit();
    } else if (args.info) {
        printDebugInfo()
    } else {
        if(args.window !== false) {
            main()
        }
    }
})

if (process.platform === 'darwin') {
    hideInsteadOfQuit = true
    Electron.app.on('before-quit', function() {
        hideInsteadOfQuit = false
    })
}



// =============================
// === Deprecations & Fixmes ===
// =============================

/// FIXME: Will not be needed in Electron 9 anymore.
Electron.app.allowRendererProcessReuse = true

// FIXME Enable Metal backend on MacOS https://github.com/electron/electron/issues/22465

// TODO[WD] Windows vibrancy
// https://github.com/fstudio/clangbuilder/issues/39
// https://github.com/Microsoft/vscode/issues/32257
// https://github.com/arkenthera/electron-vibrancy/issues/21

// TODO[WD] Window corner radius
// https://github.com/electron/electron/issues/22542
