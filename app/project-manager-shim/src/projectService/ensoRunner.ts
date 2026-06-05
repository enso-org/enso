import { createDeferred, type Deferred } from 'enso-common/src/utilities/async'
import {
  WatchedChildProcess,
  type UnexpectedExitInfo,
} from 'enso-common/src/utilities/childProcess'
import extractZip from 'extract-zip'
import * as childProcess from 'node:child_process'
import * as fs from 'node:fs'
import { createWriteStream } from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import { pipeline } from 'node:stream/promises'
import * as portfinder from 'portfinder'
import { extract } from 'tar'
import { Path } from './types.js'

const HEALTHCHECK_FAILURES_TO_RESTART = 3
const LOAD_HEALTHCHECK_INTERVAL = 250
const WATCHDOG_HEALTHCHECK_INTERVAL = 3000
const LANGUAGE_SERVER_CRASH_LOOP_MAX = 5
const LANGUAGE_SERVER_CRASH_LOOP_WINDOW_MS = 60_000

export interface Runner {
  runProject(projectPath: Path, extraEnv?: readonly (readonly [string, string])[]): Promise<number>
  createProject(path: Path, name: string, projectTemplate?: string): Promise<void>
  openProject(
    projectPath: Path,
    projectId: string,
    extraArgs?: readonly string[],
    extraEnv?: readonly (readonly [string, string])[],
  ): Promise<LanguageServerSockets>
  closeProject(projectPath: Path): Promise<void>
  isProjectRunning(projectPath: Path): Promise<boolean>
  renameProject(
    projectPath: Path,
    namespace: string,
    oldPackage: string,
    newPackage: string,
  ): Promise<void>
  registerShutdownHook(
    projectPath: Path,
    hookType: ShutdownHookType,
    hook: () => Promise<void>,
  ): Promise<void>
  version(): Promise<string>
}

export interface LanguageServerSockets {
  readonly jsonSocket: Socket
  readonly secureJsonSocket?: Socket
  readonly ydocSocket: Socket
}

export interface Socket {
  readonly host: string
  readonly port: number
}

/**
 * Use declaration merging to allow extension of ShutdownHookRegistry in other modules.
 * This enables adding new shutdown hook types without modifying the original interface.
 *
 * For example, in another module, you can add:
 * ```ts
 * declare module './projectService/ensoRunner.js' {
 *   interface ShutdownHookRegistry {
 *     'my-new-hook-type': true
 *   }
 * }
 * ```
 */
export interface ShutdownHookRegistry {
  'rename-project-directory': true
  'remove-from-list': true
}

export type ShutdownHookType = keyof ShutdownHookRegistry

const DEFAULT_JSONRPC_PORT = 30616
const LANGUAGE_SERVER_STARTUP_TIMEOUT = 30000

const TERMINATE_TIMEOUT_MS = 10_000

class OpenedProject {
  loaded: Promise<void>
  shutdownHooks: Map<ShutdownHookType, () => void | Promise<void>> = new Map()
  private closed = false
  private nextWatchdogCheck: ReturnType<typeof setTimeout> | undefined
  private readonly watcher: WatchedChildProcess
  private loadedDeferred: Deferred<void> = createDeferred()

  private constructor(
    private path: Path,
    public sockets: LanguageServerSockets,
    spawner: () => Promise<childProcess.ChildProcess>,
  ) {
    this.loaded = this.loadedDeferred.promise
    this.loadedDeferred.promise.catch(() => undefined)
    this.watcher = new WatchedChildProcess(spawner, {
      onChildStarted: () => this.startLoadingRoutine(),
      onUnexpectedExit: (reason, info) => this.onUnexpectedExit(reason, info),
      crashLoopLimit: {
        maxCrashes: LANGUAGE_SERVER_CRASH_LOOP_MAX,
        windowMs: LANGUAGE_SERVER_CRASH_LOOP_WINDOW_MS,
      },
    })
  }

  static async create(
    path: Path,
    jsonPort: number,
    ydocPort: number,
    spawner: () => Promise<childProcess.ChildProcess>,
  ) {
    const sockets = {
      jsonSocket: { host: '127.0.0.1', port: jsonPort },
      ydocSocket: { host: '127.0.0.1', port: ydocPort },
    }
    const project = new OpenedProject(path, sockets, spawner)
    // Surface synchronous spawn failures (bad executable path, etc.) to the caller.
    await project.watcher.firstSpawn
    return project
  }

  async close() {
    console.log('Closing Project', this.path)
    this.closed = true
    clearTimeout(this.nextWatchdogCheck)
    await this.gracefulShutdown()

    for (const [hookType, hook] of this.shutdownHooks) {
      try {
        this.shutdownHooks.delete(hookType)
        await hook()
      } catch (error) {
        console.error(
          `Error executing shutdown hook '${hookType}' for project ${this.path}:`,
          error,
        )
      }
    }
  }

  private startLoadingRoutine(): void {
    const deferred = createDeferred<void>()
    deferred.promise.catch(() => undefined)
    this.loadedDeferred = deferred
    this.loaded = deferred.promise
    // Capture the *specific* handle this attempt is loading, so a stale timeout fired after a
    // respawn doesn't kill the new child or reject the new deferred.
    const handle = this.watcher.current
    let resolved = false

    const healthCheck = async () => {
      if (this.closed || resolved) return
      if (await this.checkServerHealth()) {
        resolved = true
        this.startWatchdog()
        deferred.resolve()
        return
      }
      // Not using setInterval to not pile slow-responding healthchecks.
      setTimeout(healthCheck, LOAD_HEALTHCHECK_INTERVAL)
    }
    setTimeout(healthCheck, 250)

    // Timeout if server doesn't start (skip timeout in debug mode)
    const javaToolOptions = process.env.JAVA_TOOL_OPTIONS
    const isDebugMode = javaToolOptions?.includes('jdwp')
    if (!isDebugMode) {
      setTimeout(() => {
        if (resolved || this.closed) return
        // SIGKILL the *specific* child this routine was waiting on. The watcher's
        // onUnexpectedExit decides whether to respawn (which it does by default).
        handle?.child.kill('SIGKILL')
        deferred.reject(new Error('Language server startup timeout'))
      }, LANGUAGE_SERVER_STARTUP_TIMEOUT)
    }
  }

  private onUnexpectedExit(reason: string, info: UnexpectedExitInfo): boolean | undefined {
    clearTimeout(this.nextWatchdogCheck)
    if (this.closed) {
      // Exit caused by our own gracefulShutdown — suppress respawn so the port frees up.
      return false
    }
    this.loadedDeferred.reject(new Error(reason))
    if (info.exceedsCrashLimit) {
      console.error(
        `Language Server for project ${this.path} crashed ${LANGUAGE_SERVER_CRASH_LOOP_MAX} times within ${LANGUAGE_SERVER_CRASH_LOOP_WINDOW_MS}ms (${reason}); auto-respawn suspended.`,
      )
      // Defer to the watcher's default for exceedsCrashLimit (suspend). The healthcheck loop
      // is no longer scheduled, so nothing will respawn until something external triggers it.
      return
    }
    console.error(
      `Language Server process for project ${this.path} exited unexpectedly (${reason}); restarting`,
    )
  }

  private startWatchdog(): void {
    let failures = 0
    const check = async () => {
      if (this.closed) return
      if (await this.checkServerHealth()) {
        failures = 0
      } else {
        console.error('Healthcheck failed! Project:', this.path)
        failures += 1
      }
      if (failures >= HEALTHCHECK_FAILURES_TO_RESTART) {
        console.error(
          `Healthcheck of ${this.path} failed ${HEALTHCHECK_FAILURES_TO_RESTART} times in a row, restarting.`,
        )
        // The respawn triggers `onUnexpectedExit` followed by `onChildStarted`, which restarts
        // this watchdog loop on the new child. Don't schedule another `check` here.
        void this.watcher.respawn(TERMINATE_TIMEOUT_MS)
      } else {
        // Not using setInterval to not pile slow-responding healthchecks.
        this.nextWatchdogCheck = setTimeout(check, WATCHDOG_HEALTHCHECK_INTERVAL)
      }
    }
    this.nextWatchdogCheck = setTimeout(check, WATCHDOG_HEALTHCHECK_INTERVAL)
  }

  /**
   * Trigger the language server's graceful shutdown protocol (a single newline written to
   * stdin) and wait for the child to exit. SIGKILL after a timeout if it doesn't.
   */
  private async gracefulShutdown(): Promise<void> {
    const handle = this.watcher.current
    if (!handle?.alive) return
    console.log('Terminating language server process of', this.path)
    if (handle.child.stdin && !handle.child.stdin.destroyed) {
      handle.child.stdin.write('\n')
      const killTimeout = setTimeout(() => {
        if (handle.alive) {
          console.error('Language Server process of', this.path, "didn't finish in time. Killing.")
          handle.child.kill('SIGKILL')
        }
      }, TERMINATE_TIMEOUT_MS)
      try {
        await handle.waitForExit()
      } finally {
        clearTimeout(killTimeout)
      }
      console.log('Language server process of', this.path, 'exited')
      return
    }
    // No stdin to write to — fall back to SIGTERM with SIGKILL after timeout.
    await handle.terminate({ timeoutMs: TERMINATE_TIMEOUT_MS })
  }

  // Health check function
  async checkServerHealth(): Promise<boolean> {
    try {
      const response = await fetch(
        `http://${this.sockets.jsonSocket.host}:${this.sockets.jsonSocket.port}/_health`,
      )
      return response.ok
    } catch {
      return false
    }
  }
}

/** Implementation of Runner that uses the Enso executable. */
export class EnsoRunner implements Runner {
  private runningProjects = new Map<Path, OpenedProject>()
  private loadingProjects = new Map<Path, Promise<OpenedProject>>()

  /** Creates a new EnsoRunner with the path to the Enso executable. */
  constructor(private ensoPath: Path) {}

  private async runProcess<T extends childProcess.ChildProcess>(
    args: readonly string[],
    spawnCallback: (cmd: string, cmdArgs: readonly string[]) => T,
  ) {
    const [cmd, cmdArgs] =
      this.ensoPath.endsWith('.bat') ?
        ['cmd.exe', ['/c', this.ensoPath, ...args]]
      : [this.ensoPath, args]
    const isDevMode = process.env.NODE_ENV === 'development'
    if (isDevMode) {
      console.log('runProcess', cmd, cmdArgs.join(' '))
    }
    return spawnCallback(cmd, cmdArgs)
  }

  private async runCommand(
    args: readonly string[],
    options?: childProcess.SpawnOptionsWithoutStdio,
  ): Promise<void> {
    const process = await this.runProcess(args, (cmd, cmdArgs) =>
      childProcess.spawn(cmd, cmdArgs, options),
    )
    return new Promise((resolve, reject) => {
      let stdout = ''
      let stderr = ''

      process.stdout.on('data', (data) => {
        stdout += data.toString()
      })

      process.stderr.on('data', (data) => {
        stderr += data.toString()
      })

      process.on('error', (error) => {
        reject(new Error(`Failed to spawn enso process: ${error.message}`))
      })

      process.on('close', (code) => {
        if (code === 0) {
          resolve()
        } else {
          reject(
            new Error(
              `Enso process exited with code ${code}.\nstdout: ${stdout}\nstderr: ${stderr}`,
            ),
          )
        }
      })
    })
  }

  /** Run an existing Enso project at the specified path. Returns the exit code of the process. */
  async runProject(
    projectPath: Path,
    extraEnv?: readonly (readonly [string, string])[],
  ): Promise<number> {
    const args = ['--run', projectPath]
    const env = { ...process.env, ...(extraEnv ? Object.fromEntries(extraEnv) : {}) }
    const cwd = path.dirname(projectPath)
    const spawnedProcess = await this.runProcess(args, (cmd, cmdArgs) =>
      childProcess.spawn(cmd, cmdArgs, { env, cwd, stdio: ['inherit', 'inherit', 'inherit'] }),
    )
    return new Promise((resolve, reject) => {
      spawnedProcess.on('error', (error) => {
        reject(new Error(`Failed to spawn enso process: ${error.message}`))
      })
      spawnedProcess.on('exit', resolve)
    })
  }

  /** Create a new Enso project at the specified path. */
  async createProject(projectPath: Path, name: string, projectTemplate?: string): Promise<void> {
    return await this.runCommand([
      '--new',
      projectPath,
      '--new-project-name',
      name,
      ...(projectTemplate ? ['--new-project-template', projectTemplate] : []),
    ])
  }

  /** Open a project and starts its language server. */
  async openProject(
    projectPath: Path,
    projectId: string,
    extraArgs?: readonly string[],
    extraEnv?: readonly (readonly [string, string])[],
  ): Promise<LanguageServerSockets> {
    // Check if the project is already running
    const runningProject = this.runningProjects.get(projectPath)
    if (runningProject) {
      return runningProject.sockets
    }
    const loadingProject = this.loadingProjects.get(projectPath)
    if (loadingProject) {
      return (await loadingProject).sockets
    }

    // Finding server ports is not reliable if another project is opening.
    while (this.loadingProjects.size > 0) {
      await this.loadingProjects.values().next().value
    }
    const openedProject = this.findServerPorts(DEFAULT_JSONRPC_PORT).then(
      async ([jsonPort, ydocPort]) => {
        const rootId = crypto.randomUUID()
        const args: readonly string[] = [
          '--server',
          '--root-id',
          rootId,
          '--project-id',
          projectId,
          '--path',
          projectPath,
          '--interface',
          '127.0.0.1',
          '--rpc-port',
          jsonPort.toString(),
          ...(extraArgs ?? []),
        ]

        const env = {
          ...process.env,
          LANGUAGE_SERVER_YDOC_PORT: ydocPort.toString(),
          ...(extraEnv ? Object.fromEntries(extraEnv) : {}),
        }

        const cwd = path.dirname(projectPath)
        const project = await OpenedProject.create(projectPath, jsonPort, ydocPort, () =>
          this.runProcess(args, (cmd, cmdArgs) =>
            childProcess.spawn(cmd, cmdArgs, {
              env,
              detached: false,
              cwd,
              stdio: ['pipe', 'inherit', 'inherit'],
              windowsHide: true,
            }),
          ),
        )
        project.shutdownHooks.set('remove-from-list', () => {
          this.runningProjects.delete(projectPath)
          this.loadingProjects.delete(projectPath)
        })
        return project.loaded.then(() => project)
      },
    )

    this.loadingProjects.set(projectPath, openedProject)
    openedProject.then((project) => {
      this.runningProjects.set(projectPath, project)
    })
    openedProject.finally(() => {
      this.loadingProjects.delete(projectPath)
    })
    return openedProject.then((project) => project.sockets)
  }

  /** Closes a project and stops its language server. */
  async closeProject(projectPath: Path): Promise<void> {
    // First wait for potential initialization end.
    await this.loadingProjects.get(projectPath)
    const runningProject = this.runningProjects.get(projectPath)

    if (!runningProject) {
      // Project is not running or already closed
      return
    }

    return runningProject.close()
  }

  /** Checks if a project's language server is currently running. */
  async isProjectRunning(projectPath: Path): Promise<boolean> {
    return this.runningProjects.has(projectPath)
  }

  /** Registers an action to be executed when the project is closed. */
  async registerShutdownHook(
    projectPath: Path,
    hookType: ShutdownHookType,
    hook: () => void | Promise<void>,
  ): Promise<void> {
    const runningProject = this.runningProjects.get(projectPath)

    if (!runningProject) {
      // If project is not running, execute the hook immediately
      await hook()
      return
    }

    runningProject.shutdownHooks.set(hookType, hook)
  }

  /**
   * Renames the running language server project.
   *
   * It does _not_ rename it's directory.
   */
  async renameProject(
    projectPath: Path,
    namespace: string,
    oldPackage: string,
    newPackage: string,
  ): Promise<void> {
    const runningProject = this.runningProjects.get(projectPath)
    if (!runningProject) {
      throw new Error(`Project ${projectPath} is not running`)
    }

    const { sockets } = runningProject
    const requestBody = {
      namespace: namespace,
      oldName: oldPackage,
      newName: newPackage,
    }

    try {
      // Send POST request to the language server's rename endpoint
      const response = await fetch(
        `http://127.0.0.1:${sockets.jsonSocket.port}/refactoring/renameProject`,
        {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
          },
          body: JSON.stringify(requestBody),
        },
      )

      if (!response.ok) {
        const errorBody = await response.text()
        let errorMessage = `Failed to rename project: ${response.status} ${response.statusText}`
        try {
          const errorJson = JSON.parse(errorBody)
          if (errorJson.error) {
            errorMessage = `Failed to rename project: ${errorJson.error}`
          }
        } catch {
          if (errorBody) {
            errorMessage += ` - ${errorBody}`
          }
        }
        throw new Error(errorMessage)
      }
    } catch (error) {
      if (error instanceof Error) {
        throw error
      } else {
        throw new Error(`Failed to rename project: ${error}`)
      }
    }
  }

  /** Gets the version of the Enso executable. */
  async version(): Promise<string> {
    return new Promise((resolve, reject) => {
      const args = ['--version']
      const cmd = this.ensoPath.endsWith('.bat') ? 'cmd.exe' : this.ensoPath
      const cmdArgs = this.ensoPath.endsWith('.bat') ? ['/c', this.ensoPath, ...args] : args
      const process = childProcess.spawn(cmd, cmdArgs)

      let stdout = ''
      let stderr = ''

      process.stdout.on('data', (data) => {
        stdout += data.toString()
      })

      process.stderr.on('data', (data) => {
        stderr += data.toString()
      })

      process.on('error', (error) => {
        reject(new Error(`Failed to spawn enso process: ${error.message}`))
      })

      process.on('close', (code) => {
        if (code === 0) {
          resolve(stdout.trim())
        } else {
          reject(new Error(`Enso process exited with code ${code}. stderr: ${stderr}`))
        }
      })
    })
  }

  /** Finds an available port starting from the given port number. */
  private async findServerPorts(startPort: number): Promise<[number, number]> {
    return new Promise((resolve, reject) => {
      portfinder.getPorts(2, { port: startPort }, (err, ports) => {
        if (err) {
          reject(new Error(`Failed to find ports: ${err}`))
        }
        if (ports.length < 2) {
          reject(new Error(`Failed to find all ports: ${ports}`))
        }
        resolve(ports as [number, number])
      })
    })
  }
}

function checkExecutable(filePath: string) {
  try {
    fs.accessSync(filePath, fs.constants.X_OK)
  } catch {
    throw new Error(`Enso executable at ${filePath} is not executable`)
  }
  return Path(filePath)
}

const ensoExecutables = (() => {
  switch (os.platform()) {
    case 'win32': {
      return ['enso.exe', 'enso.bat']
    }
    case 'darwin':
    case 'linux':
    default: {
      return ['enso']
    }
  }
})()

function checkExecutables(...segments: readonly string[]): Path | undefined {
  if (!segments.includes('*')) {
    for (const ensoExecutable of ensoExecutables) {
      const ensoPath = path.join(...segments, ensoExecutable)
      try {
        fs.accessSync(ensoPath)
        return checkExecutable(ensoPath)
      } catch {
        // File doesn't exist, continue searching
      }
    }
    return
  }
  const literalSegments: string[] = []
  let i = -1
  for (const segment of segments) {
    i += 1
    if (segment === '*') {
      const basePath = path.join(...literalSegments)
      try {
        for (const entry of fs.readdirSync(basePath)) {
          const result = checkExecutables(basePath, entry, ...segments.slice(i + 1))
          if (result) {
            return result
          }
        }
      } catch {
        // Directory doesn't exist, continue searching
      }
    } else {
      literalSegments.push(segment)
    }
  }
  return
}

/** Find the path to the `enso` executable. */
export function findEnsoExecutable(workDir: string = '.'): Path | undefined {
  workDir = path.resolve(workDir)

  // Check ENSO_ENGINE_PATH environment variable first
  const envPath = process.env.ENSO_ENGINE_PATH
  if (envPath) {
    try {
      fs.accessSync(envPath)
      return checkExecutable(envPath)
    } catch {
      // File doesn't exist, continue searching
    }
  }

  const executablePath = process.argv[0] ? path.dirname(process.argv[0]) : undefined
  const directories: readonly (readonly string[])[] = [
    // Check executable path
    ...(executablePath ? [[executablePath, 'resources', 'enso', 'dist', '*', 'bin']] : []),
    // Check executable path for MacOs
    ...(executablePath ? [[executablePath, '..', 'Resources', 'enso', 'dist', '*', 'bin']] : []),
    // Check enso/dist/*/bin/enso
    [workDir, 'enso', 'dist', '*', 'bin'],
    // Check built-distribution/*/enso/dist/*/bin/enso
    [workDir, 'built-distribution', '*', 'enso', 'dist', '*', 'bin'],
    // Check built-distribution/*/*/bin/enso
    [workDir, 'built-distribution', '*', '*', 'bin'],
    // Macos dist/backend/dist/*/bin nightly
    [workDir, 'dist', 'backend', 'dist', '*', 'bin'],
  ]

  for (const directory of directories) {
    const result = checkExecutables(...directory)
    if (result) {
      return result
    }
  }
}

/**
 * Bundled engine's `lib/Standard` directory, derived from the engine binary's location.
 * `undefined` when the binary cannot be located or the derived directory does not exist.
 */
export function findStdlibRoot(workDir: string = '.'): Path | undefined {
  const ensoPath = findEnsoExecutable(workDir)
  if (!ensoPath) return undefined
  const engineRoot = path.dirname(path.dirname(ensoPath))
  const stdlib = path.join(engineRoot, 'lib', 'Standard')
  try {
    fs.accessSync(stdlib)
  } catch {
    return undefined
  }
  return Path(stdlib)
}

/**
 * Downloads the latest Enso engine from GitHub.
 *
 * This function automatically detects the current platform (macOS, Linux, or Windows)
 * and architecture (amd64 or aarch64) to download the appropriate engine binary.
 * The engine is downloaded from GitHub and extracted to the built-distribution directory.
 *
 * The type of release to download is controlled by the DOWNLOAD_ENSO_RUNNER environment variable:
 * - If set to 'release': downloads the latest stable release
 * - If set to 'prerelease' or not set: downloads the latest prerelease
 * @param projectRoot - The root directory of the project where the engine will be installed
 * @returns A promise that resolves to the path where the engine was extracted
 */
export async function downloadEnsoEngine(projectRoot: string): Promise<string> {
  // Check if we should download release or prerelease
  const downloadType = process.env.DOWNLOAD_ENSO_RUNNER
  const useRelease = downloadType === 'release'

  console.log(`Downloading latest Enso engine (${useRelease ? 'release' : 'prerelease'})...`)

  // Determine platform and architecture
  const platform = os.platform()
  const arch = os.arch()

  let platformString: string
  let extensionString: string
  if (platform === 'darwin') {
    platformString = 'macos'
    extensionString = '.tar.gz'
  } else if (platform === 'linux') {
    platformString = 'linux'
    extensionString = '.tar.gz'
  } else if (platform === 'win32') {
    platformString = 'windows'
    extensionString = '.zip'
  } else {
    throw new Error(`Unsupported platform: ${platform}`)
  }

  let archString: string
  if (arch === 'x64') {
    archString = 'amd64'
  } else if (arch === 'arm64') {
    archString = 'aarch64'
  } else {
    throw new Error(`Unsupported architecture: ${arch}`)
  }

  // Fetch all releases from GitHub API
  const releasesUrl = 'https://api.github.com/repos/enso-org/enso/releases'
  const headers: HeadersInit = {}
  if (process.env.GITHUB_TOKEN) {
    headers['Authorization'] = `Bearer ${process.env.GITHUB_TOKEN}`
  }
  const releasesResponse = await fetch(releasesUrl, { headers })

  if (!releasesResponse.ok) {
    throw new Error(`Failed to fetch releases: ${releasesResponse.statusText}`)
  }

  const releases = await releasesResponse.json()

  // Filter based on whether we want releases or prereleases
  const targetReleases =
    useRelease ?
      releases.filter((release: any) => !release.prerelease)
    : releases.filter((release: any) => release.prerelease)

  if (targetReleases.length === 0) {
    throw new Error(`No ${useRelease ? 'releases' : 'prereleases'} found`)
  }

  let releaseData: any = null
  let asset: any = null
  let assetName: string = ''

  // Iterate through target releases to find one with matching asset
  for (const targetRelease of targetReleases) {
    const version = targetRelease.tag_name
    assetName = `enso-bundle-${version}-${platformString}-${archString}${extensionString}`
    asset = targetRelease.assets.find((a: any) => a.name === assetName)

    if (asset) {
      releaseData = targetRelease
      break
    }
  }

  if (!releaseData || !asset) {
    throw new Error(
      `Could not find asset: enso-engine-*-${platformString}-${archString}${extensionString} in any ${useRelease ? 'release' : 'prerelease'}`,
    )
  }

  console.log(`Downloading ${assetName}...`)

  // Download the asset
  const downloadResponse = await fetch(asset.url, {
    headers: {
      ...headers,
      Accept: 'application/octet-stream',
    },
  })

  if (!downloadResponse.ok) {
    throw new Error(`Failed to download asset: ${downloadResponse.statusText}`)
  }

  // Create the built-distribution directory if it doesn't exist
  const distDir = path.join(projectRoot, 'built-distribution')
  if (!fs.existsSync(distDir)) {
    fs.mkdirSync(distDir, { recursive: true })
  }

  // Save and extract the archive
  const archivePath = path.join(distDir, assetName)
  const extractDir = path.join(distDir, assetName.replace(extensionString, ''))

  // Create extract directory if it doesn't exist
  if (!fs.existsSync(extractDir)) {
    fs.mkdirSync(extractDir, { recursive: true })
  }

  // Download and save the file
  const fileStream = createWriteStream(archivePath)
  await pipeline(downloadResponse.body as any, fileStream)

  console.log(`Extracting to ${extractDir}...`)

  // Extract the archive
  if (extensionString === '.tar.gz') {
    await pipeline(fs.createReadStream(archivePath), extract({ cwd: extractDir }))
  } else {
    await extractZip(archivePath, { dir: extractDir })
  }

  // Clean up the archive file
  fs.unlinkSync(archivePath)

  console.log(`Enso engine downloaded and extracted to ${extractDir}`)

  patchEnsoEngine(extractDir)

  return extractDir
}

/**
 * Patches the Enso distribution by renaming `.enso.portable` to `.enso.bundle`.
 * This is a temporary solution during the unification of portable and bundle Enso distributions.
 * @param distributionDir - The path to the enso distribution
 */
export function patchEnsoEngine(distributionDir: string): void {
  const checkAndRename = (dir: string): boolean => {
    const portableFile = path.join(dir, '.enso.portable')
    const bundleFile = path.join(dir, '.enso.bundle')

    if (fs.existsSync(portableFile)) {
      fs.renameSync(portableFile, bundleFile)
      console.log(`Renamed ${portableFile} to ${bundleFile}`)
      return true
    }
    return false
  }

  // Check the distribution directory itself
  if (checkAndRename(distributionDir)) {
    return
  }

  // Check one level down
  try {
    const entries = fs.readdirSync(distributionDir, { withFileTypes: true })
    for (const entry of entries) {
      if (entry.isDirectory()) {
        const childDir = path.join(distributionDir, entry.name)
        if (checkAndRename(childDir)) {
          return
        }
      }
    }
  } catch (error) {
    console.error(`Error scanning directory ${distributionDir}:`, error)
  }
}
