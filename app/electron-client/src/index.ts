/**
 * @file Definition of an Electron application, which entails the creation of a rudimentary HTTP
 * server and the presentation of a Chrome web view, designed for optimal performance and
 * compatibility across a wide range of hardware configurations. The application's web component
 * is then served and showcased within the web view, complemented by the establishment of an
 * Inter-Process Communication channel, which enables seamless communication between the served web
 * application and the Electron process.
 */

import './cjs-shim' // must be imported first

import type { BrowserWindowConstructorOptions, WebPreferences } from 'electron'
import { DEEP_LINK_SCHEME, PRODUCT_NAME } from 'enso-common'
import {
  buildWebAppURLSearchParamsFromArgs,
  defaultOptions,
  type Options,
} from 'enso-common/src/options'
import { EnsoPath } from 'enso-common/src/services/Backend'
import { HttpClient } from 'enso-common/src/services/HttpClient'
import { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import {
  getText as originalGetText,
  TEXTS,
  type Replacements,
  type TextId,
} from 'enso-common/src/text'
import { Path } from 'enso-common/src/utilities/file'
import { access, constants, readFile, writeFile } from 'node:fs/promises'
import { platform } from 'node:os'
import { join as joinPath } from 'node:path'
import process from 'node:process'
import { downloadSamples, runHybridProjectByUrl, runLocalProjectByPath } from 'project-manager-shim'
import { initAuthentication } from './authentication.js'
import { parseArgs } from './configParser.js'
import { VERSION } from './contentConfig.js'
import { printInfo, VERSION_INFO } from './debug.js'
import { initIpc, registerShortcuts, setChromeOptions } from './electron.js'
import {
  getFileToOpen,
  handleOpenFile,
  parseClientArguments,
  setOpenFileEventHandler,
} from './fileAssociations.js'
import { Channel } from './ipc.js'
import { setupLogger } from './log.js'
import { filterByRole, inheritMenuItem, makeMenuItem, replaceMenuItems } from './menuItems.js'
import { capitalizeFirstLetter } from './naming.js'
import { APP_PATH, ASSETS_PATH } from './paths.js'
import { handleProjectProtocol, setupProjectService, version } from './projectService.js'
import { enableAll } from './security.js'
import { Config, Server } from './server.js'
import { getUrlToOpen, handleOpenUrl, registerAssociations } from './urlAssociations.js'

type Electron = typeof import('electron')

const DEFAULT_WINDOW_WIDTH = 1380
const DEFAULT_WINDOW_HEIGHT = 900

function exit(code: number, electron: Electron | undefined) {
  if (electron) {
    electron.app.exit(code)
  } else {
    process.exit(code)
  }
}

function quit(electron: Electron | undefined) {
  if (electron) {
    electron.app.quit()
  } else {
    process.exit(0)
  }
}

function showErrorBox(title: string, content: string, electron: Electron | undefined) {
  if (electron) {
    electron.dialog.showErrorBox(title, content)
  } else {
    console.error(`${title}\n\n${content}`)
  }
}

/** Convert path to proper `file://` URL. */
function pathToURL(path: string): URL {
  if (process.platform === 'win32') {
    return new URL(encodeURI(`file:///${path.replaceAll('\\', '/')}`))
  } else {
    return new URL(encodeURI(`file://${path}`))
  }
}

/**
 * The Electron application. It is responsible for starting all the required services, and
 * displaying and managing the app window.
 */
interface App {
  window: import('electron').BrowserWindow | null
  server: Server | null
  webOptions: Options
  isQuitting: boolean
}

const createApp = (): App => ({
  window: null,
  server: null,
  webOptions: defaultOptions(),
  isQuitting: false,
})

function runElectronApp(
  electron: Electron,
  app: App,
  args: Options,
  fileToOpen: string | null,
  urlToOpen: URL | null,
) {
  registerAssociations(electron)
  // Register file associations for macOS.
  setOpenFileEventHandler((path) => {
    if (electron.app.isReady()) {
      const project = handleOpenFile(path)
      app.window?.webContents.send(Channel.openProject, project)
    } else {
      setProjectToOpenOnStartup(app, pathToURL(path), electron)
    }
  })
  const isOriginalInstance = electron.app.requestSingleInstanceLock({
    fileToOpen,
    urlToOpen,
  })
  if (isOriginalInstance) {
    handleItemOpening(app, fileToOpen, urlToOpen, electron)
    setChromeOptions(electron)
    enableAll()

    onStart(electron).catch((err) => {
      console.error(err)
    })

    electron.app.on('before-quit', () => {
      app.isQuitting = true
    })

    electron.app.on('second-instance', (_event, argv) => {
      console.error(`Got data from 'second-instance' event: '${argv.toString()}'.`)

      const isWin = platform() === 'win32'

      if (isWin) {
        const ensoLinkInArgs = argv.find((arg) => arg.startsWith(DEEP_LINK_SCHEME))

        if (ensoLinkInArgs != null) {
          electron.app.emit('open-url', new CustomEvent('open-url'), ensoLinkInArgs)
        }
      }

      // The second instances will close themselves, but our window likely is not in the
      // foreground - the focus went to the "second instance" of the application.
      if (app.window) {
        if (app.window.isMinimized()) {
          app.window.restore()
        }
        app.window.focus()
      } else {
        console.error('No window found after receiving URL from second instance.')
      }
    })
    electron.app.whenReady().then(
      async () => {
        console.log('Electron application is ready.')

        electron.protocol.handle('enso', (request) =>
          handleProjectProtocol(decodeURIComponent(request.url.replace('enso://', ''))),
        )

        await main(app, args, electron)
      },
      (error) => {
        console.error('Failed to initialize Electron.', error)
      },
    )
    registerShortcuts(electron)
  } else {
    console.log('Another instance of the application is already running, exiting.')
    quit(electron)
  }
}

/** Background tasks scheduled on the application startup. */
async function onStart(electron: Electron | undefined) {
  const writeVersionInfoPromise = (async () => {
    if (!electron) return
    const userData = electron.app.getPath('userData')
    const versionInfoPath = joinPath(userData, 'version_info.json')
    const versionInfoPathExists = await access(versionInfoPath, constants.F_OK)
      .then(() => true)
      .catch(() => false)

    if (versionInfoPathExists) {
      const versionInfoText = await readFile(versionInfoPath, 'utf8')
      const versionInfoJson = JSON.parse(versionInfoText)

      if (VERSION_INFO.version === versionInfoJson.version && !VERSION.isDev()) return
    }

    return writeFile(versionInfoPath, JSON.stringify(VERSION_INFO), 'utf8')
  })()

  const downloadSamplesPromise = downloadSamples()

  return Promise.allSettled([writeVersionInfoPromise, downloadSamplesPromise])
}

interface ParsedArguments {
  readonly args: Options
  readonly fileToOpen: string | null
  readonly urlToOpen: URL | null
}

/** Process the command line arguments. */
function processArguments(args: readonly string[]): ParsedArguments {
  // We parse only "client arguments", so we don't have to worry about the Electron-Dev vs
  // Electron-Proper distinction.
  const fileToOpen = getFileToOpen(args)
  const urlToOpen = getUrlToOpen(args)
  // If we are opening a file (i.e. we were spawned with just a path of the file to open as
  // the argument) or URL, it means that effectively we don't have any non-standard arguments.
  // We just need to let the caller know that we are opening a file.
  const argsToParse = fileToOpen != null || urlToOpen != null ? [] : args
  return { args: parseArgs(argsToParse), fileToOpen, urlToOpen }
}

/**
 * Set the project to be opened on application startup.
 *
 * This method should be called before the application is ready, as it only
 * modifies the startup options. If the application is already initialized,
 * an error will be logged, and the method will have no effect.
 * @param projectUrl - The `file://` url of the project to be opened on startup.
 */
function setProjectToOpenOnStartup(app: App, projectUrl: URL, electron: Electron) {
  // Make sure that we are not initialized yet, as this method should be called before the
  // application is ready.
  if (!electron.app.isReady()) {
    console.log(`Setting the project to open on startup to '${projectUrl}'.`)
    app.webOptions.startup.project = projectUrl.toString()
  } else {
    console.error(
      `Cannot set the project to open on startup to '${projectUrl}', as the application is already initialized.`,
    )
  }
}

/**
 * This method is invoked when the application was spawned due to being a default application
 * for a URL protocol or file extension.
 */
function handleItemOpening(
  app: App,
  fileToOpen: string | null,
  urlToOpen: URL | null,
  electron: Electron,
) {
  console.log('Opening file or URL.', { fileToOpen, urlToOpen })
  try {
    if (fileToOpen != null) {
      // The IDE must receive the project path, otherwise if the IDE has a custom root directory
      // set then it is added to the (incorrect) default root directory.
      setProjectToOpenOnStartup(app, pathToURL(fileToOpen), electron)
    }

    if (urlToOpen != null) {
      handleOpenUrl(urlToOpen)
    }
  } catch {
    // If we failed to open the file, we should enter the usual welcome screen.
    // The `handleOpenFile` function will have already displayed an error message.
  }
}

/** Main app entry point. */
async function main(app: App, args: Options, electron: Electron | undefined) {
  // We catch all errors here. Otherwise, it might be possible that the app will run partially
  // and enter a "zombie mode", where user is not aware of the app still running.
  try {
    console.log('Starting the application')
    // Note that we want to do all the actions synchronously, so when the window
    // appears, it serves the website immediately.
    await startContentServerIfEnabled(app, args)
    await createWindowIfEnabled(app, args, electron)
    initIpc(app.window)
    await loadWindowContent(app, args)
    if (electron) {
      /**
       * The non-null assertion on the following line is safe because the window
       * initialization is guarded by the `createWindowIfEnabled` method. The window is
       * not yet created at this point, but it will be created by the time the
       * authentication module uses the lambda providing the window.
       */
      initAuthentication(electron, () => app.window!)
    }
  } catch (err) {
    console.error('Failed to initialize the application, shutting down. Error: ', err)
    quit(electron)
  }
}

/** Setup the project service. */
function createProjectService(args: Options) {
  const backendVerboseOpts = args.debug.verbose ? ['--log-level', 'trace'] : []
  const backendProfileTime = ['--profiling-time', String(args.debug.profileTime)]
  const backendProfileOpts =
    args.debug.profile ? ['--profiling-path', 'profiling.npss', ...backendProfileTime] : []
  const backendJvmOpts = args.useJvm ? ['--jvm'] : []
  const backendOpts = [...backendVerboseOpts, ...backendProfileOpts, ...backendJvmOpts]

  return setupProjectService(backendOpts)
}

/** Start the content server, which will serve the application content (HTML) to the window. */
async function startContentServerIfEnabled(app: App, args: Options) {
  if (!args.useServer) return
  console.log('Starting the content server.')
  const serverCfg = new Config({
    dir: ASSETS_PATH,
    port: args.server.port,
  })
  const projectService = createProjectService(args)
  app.server = await Server.create(serverCfg, projectService)
  console.log('Content server started.')
}

/** Create the Electron window and display it on the screen. */
async function createWindowIfEnabled(app: App, args: Options, electron: Electron | undefined) {
  if (!args.displayWindow) return
  if (!electron) {
    console.error('Running in headless mode, window will not be created.')
    return
  }
  console.log('Creating the window.')
  const webPreferences: WebPreferences = {
    preload: joinPath(APP_PATH, 'preload.mjs'),
    sandbox: true,
    spellcheck: false,
    ...(process.env.ENSO_TEST ? { partition: 'test' } : {}),
  }
  const windowPreferences: BrowserWindowConstructorOptions = {
    webPreferences,
    width: DEFAULT_WINDOW_WIDTH,
    height: DEFAULT_WINDOW_HEIGHT,
    frame: true,
    titleBarStyle: 'default',
    ...(process.env.DEV_DARK_BACKGROUND ? { backgroundColor: '#36312c' } : {}),
  }
  const window = new electron.BrowserWindow(windowPreferences)

  const oldMenu = electron.Menu.getApplicationMenu()
  if (oldMenu != null) {
    const newMenu = replaceMenuItems(oldMenu.items, [
      {
        filter: [filterByRole('help')],
        replacement: (item) =>
          inheritMenuItem(item, undefined, [
            makeMenuItem(window, `About ${PRODUCT_NAME}`, 'about'),
          ]),
      },
      {
        filter: [filterByRole('fileMenu'), filterByRole('close')],
        replacement: () => makeMenuItem(window, 'Close Tab', 'closeTab', 'CmdOrCtrl+W'),
      },
      {
        filter: [filterByRole('appMenu'), filterByRole('about')],
        replacement: () => undefined,
      },
      {
        filter: [filterByRole('appMenu'), filterByRole('hide')],
        replacement: (item) => inheritMenuItem(item, `Hide ${PRODUCT_NAME}`),
      },
      {
        filter: [filterByRole('appMenu'), filterByRole('quit')],
        replacement: (item) => inheritMenuItem(item, `Quit ${PRODUCT_NAME}`),
      },
    ])
    electron.Menu.setApplicationMenu(newMenu)
  }
  window.setMenuBarVisibility(false)

  if (args.debug.devTools) {
    window.webContents.openDevTools()
  }

  const allowedPermissions = ['clipboard-read', 'clipboard-sanitized-write']
  window.webContents.session.setPermissionRequestHandler((_webContents, permission, callback) => {
    if (allowedPermissions.includes(permission)) {
      callback(true)
    } else {
      console.error(`Denied permission check '${permission}'.`)
      callback(false)
    }
  })

  // Quit application on window close on all platforms except Mac (it is default behavior on Mac).
  const closeToQuit = process.platform !== 'darwin'

  window.on('close', (event) => {
    if (!app.isQuitting && !closeToQuit) {
      event.preventDefault()
      window.hide()
    }
  })

  electron.app.on('activate', () => {
    if (!closeToQuit) {
      window.show()
    }
  })

  window.webContents.on('render-process-gone', (_event, details) => {
    console.error('Error, the render process crashed.', details)
  })

  app.window = window
  console.log('Window created.')
}

/**
 * The server port. In case the server was not started, the port specified in the configuration
 * is returned. This might be used to connect this application window to another, existing
 * application server.
 */
function serverPort(app: App, args: Options): number {
  return app.server?.config.port ?? args.server.port
}

/** Redirect the web view to `localhost:<port>` to see the served website. */
async function loadWindowContent(app: App, args: Options) {
  if (!app.window) return
  const searchParams = buildWebAppURLSearchParamsFromArgs({
    ...app.webOptions,
    ...args,
  })
  const address = new URL('https://localhost')
  address.port = serverPort(app, args).toString()
  address.search = searchParams.toString()
  console.log(`Loading the window address '${address.toString()}'.`)
  if (process.env.ELECTRON_DEV_MODE === 'true') {
    // Vite takes a while to be `import`ed, so the first load almost always fails.
    // Reload every second until Vite is ready
    // (i.e. when `index.html` has a non-empty body).
    const window = app.window
    const onLoad = () => {
      void window.webContents.mainFrame
        // Get the HTML contents of `document.body`.
        .executeJavaScript('document.body.innerHTML')
        .then((html) => {
          // If `document.body` is empty, then `index.html` failed to load.
          if (html === '') {
            console.warn('Loading failed, reloading...')
            window.webContents.once('did-finish-load', onLoad)
            setTimeout(() => {
              void window.loadURL(address.toString())
            }, 1_000)
          }
        })
    }
    // Wait for page to load before checking content, because of course the content is
    // empty if the page isn't loaded.
    window.webContents.once('did-finish-load', onLoad)
  }
  await app.window.loadURL(address.toString())
}

/** Print the version of the frontend and the backend. */
async function printVersion(): Promise<void> {
  const indent = '    '
  let maxNameLen = 0
  for (const name in VERSION_INFO) {
    maxNameLen = Math.max(maxNameLen, name.length)
  }
  process.stdout.write('Frontend:\n')
  for (const [name, value] of Object.entries(VERSION_INFO)) {
    const label = capitalizeFirstLetter(name)
    const spacing = ' '.repeat(maxNameLen - name.length)
    process.stdout.write(`${indent}${label}:${spacing} ${value}\n`)
  }
  process.stdout.write('\n')
  process.stdout.write('Backend:\n')
  const backend = await version()
  const lines = backend.split(/\r?\n/).filter((line) => line.length > 0)
  for (const line of lines) {
    process.stdout.write(`${indent}${line}\n`)
  }
}

/**
 * A function that gets localized text for a given key, with optional replacements.
 * @param key - The key of the text to get.
 * @param replacements - The replacements to insert into the text.
 * If the text contains placeholders like `$0`, `$1`, etc.,
 * they will be replaced with the corresponding replacement.
 */
export type GetText = <K extends TextId>(key: K, ...replacements: Replacements[K]) => string

const getText: GetText = (key, ...replacements) => {
  return originalGetText(TEXTS.english, key, ...replacements)
}

function createRemoteBackend() {
  // TODO: pass authentication headers to `HttpClient` constructor
  const httpClient = new HttpClient()
  const downloader = () => {
    // TODO: implement downloading (low priority but might as well do it now)
  }
  const baseUrl = new URL('https://api.cloud.enso.org')
  return new RemoteBackend(getText, httpClient, downloader, baseUrl)
}

/** Initialize and run the Electron application. */
async function runApp(app: App, parsedArguments: ParsedArguments, electron: Electron | undefined) {
  const { args, fileToOpen, urlToOpen } = parsedArguments
  process.on('uncaughtException', (err, origin) => {
    console.error(`Uncaught exception: ${err.toString()}\nException origin: ${origin}`)
    showErrorBox(PRODUCT_NAME, err.stack ?? err.toString(), electron)
    exit(1, electron)
  })
  setupLogger()
  if (args.version) {
    await printVersion()
    return quit(electron)
  } else if (args.debug.info) {
    await electron?.app.whenReady()
    await printInfo()
    return quit(electron)
  } else if (electron) {
    runElectronApp(electron, app, args, fileToOpen, urlToOpen)
  } else if (args.headless) {
    const projectToOpen = args.startup.project
    if (projectToOpen.startsWith(`${DEEP_LINK_SCHEME}:`)) {
      try {
        await runHybridProjectByUrl(EnsoPath(projectToOpen.toString()), createRemoteBackend())
      } catch (error) {
        console.error(`Error starting hybrid project '${projectToOpen}':`, error)
        return exit(1, electron)
      }
    } else if (projectToOpen) {
      try {
        await runLocalProjectByPath(Path(projectToOpen))
      } catch (error) {
        console.error(`Error starting local project '${projectToOpen}':`, error)
        return exit(1, electron)
      }
    } else {
      console.error('Usage: `--headless --startup.project <project path or url>`')
      return exit(1, electron)
    }
  }
}

const app = createApp()
const clientArguments = parseClientArguments(process.argv)
const parsedArguments = processArguments(clientArguments)
if (parsedArguments.args.headless) {
  void runApp(app, parsedArguments, undefined)
} else {
  const electron = await import('electron')
  void runApp(app, parsedArguments, electron)
}
