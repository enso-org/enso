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

export interface Runner {
  runProject(projectPath: Path, extraEnv?: readonly (readonly [string, string])[]): Promise<void>
  createProject(path: Path, name: string, projectTemplate?: string): Promise<void>
  openProject(
    projectPath: Path,
    projectId: string,
    extraArgs?: readonly string[],
    extraEnv?: readonly (readonly [string, string])[],
  ): Promise<LanguageServerSockets>
  closeProject(projectId: string): Promise<void>
  isProjectRunning(projectId: string): Promise<boolean>
  renameProject(
    projectId: string,
    namespace: string,
    oldPackage: string,
    newPackage: string,
  ): Promise<void>
  registerShutdownHook(
    projectId: string,
    hookType: ShutdownHookType,
    hook: () => Promise<void>,
  ): Promise<void>
  version(): Promise<string>
}

export interface LanguageServerSockets {
  readonly jsonSocket: Socket
  readonly secureJsonSocket?: Socket
  readonly binarySocket: Socket
  readonly secureBinarySocket?: Socket
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
}

export type ShutdownHookType = keyof ShutdownHookRegistry

interface RunningProject {
  process: childProcess.ChildProcess
  sockets: LanguageServerSockets
  shutdownHooks: Map<ShutdownHookType, () => void | Promise<void>>
}

const DEFAULT_JSONRPC_PORT = 30616
const LANGUAGE_SERVER_STARTUP_TIMEOUT = 30000

/** Implementation of Runner that uses the Enso executable. */
export class EnsoRunner implements Runner {
  private runningProjects = new Map<string, RunningProject>()
  private loadingProjects = new Map<string, Promise<LanguageServerSockets>>()

  /** Creates a new EnsoRunner with the path to the Enso executable. */
  constructor(private ensoPath: Path) {}

  private async runProcess<T extends childProcess.ChildProcess>(
    args: readonly string[],
    spawnCallback: (cmd: string, cmdArgs: readonly string[]) => T,
  ) {
    const cmd = this.ensoPath.endsWith('.bat') ? 'cmd.exe' : this.ensoPath
    const cmdArgs = this.ensoPath.endsWith('.bat') ? ['/c', this.ensoPath, ...args] : args
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

  /** Run an existing Enso project at the specified path. */
  async runProject(
    projectPath: Path,
    extraEnv?: readonly (readonly [string, string])[],
  ): Promise<void> {
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
      spawnedProcess.on('exit', (code) => {
        if (code === 0) {
          resolve()
        } else {
          reject(new Error(`Enso process exited with code ${code}.`))
        }
      })
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
    const runningProject = this.runningProjects.get(projectId)
    if (runningProject) {
      return runningProject.sockets
    }
    const loadingProject = this.loadingProjects.get(projectId)
    if (loadingProject) {
      return loadingProject
    }

    // Finding server ports is not reliable if another project is opening.
    while (this.loadingProjects.size > 0) {
      await this.loadingProjects.values().next().value
    }
    const promise = this.findServerPorts(DEFAULT_JSONRPC_PORT).then(
      async ([jsonPort, binaryPort, ydocPort]) => {
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
          '--data-port',
          binaryPort.toString(),
          ...(extraArgs ?? []),
        ]

        const env = {
          ...process.env,
          LANGUAGE_SERVER_YDOC_PORT: ydocPort.toString(),
          ...(extraEnv ? Object.fromEntries(extraEnv) : {}),
        }

        const cwd = path.dirname(projectPath)
        const serverProcess = await this.runProcess(args, (cmd, cmdArgs) =>
          childProcess.spawn(cmd, cmdArgs, {
            env,
            detached: false,
            cwd,
            stdio: ['pipe', 'inherit', 'inherit'],
            windowsHide: true,
          }),
        )

        return new Promise<LanguageServerSockets>((resolve, reject) => {
          let resolved = false

          // Health check function
          const checkServerHealth = async (): Promise<boolean> => {
            try {
              const response = await fetch(`http://127.0.0.1:${jsonPort}/_health`)
              return response.ok
            } catch {
              return false
            }
          }

          // Start polling for server readiness after initial delay
          const startHealthCheck = () => {
            const pollInterval = setInterval(async () => {
              if (resolved) {
                clearInterval(pollInterval)
                return
              }

              const isReady = await checkServerHealth()
              if (isReady) {
                clearInterval(pollInterval)
                resolved = true
                const sockets: LanguageServerSockets = {
                  jsonSocket: { host: '127.0.0.1', port: jsonPort },
                  binarySocket: { host: '127.0.0.1', port: binaryPort },
                  ydocSocket: { host: '127.0.0.1', port: ydocPort },
                }
                this.runningProjects.set(projectId, {
                  process: serverProcess,
                  sockets: sockets,
                  shutdownHooks: new Map(),
                })
                resolve(sockets)
              }
            }, 250) // Poll every 250ms
          }

          // Start health check after initial delay
          setTimeout(startHealthCheck, 250)

          serverProcess.on('error', (error) => {
            console.error(error.toString())
            if (!resolved) {
              reject(new Error(`Failed to start language server: ${error.message}`))
            }
          })

          serverProcess.on('close', async (code) => {
            // Execute shutdown hooks if the process exits unexpectedly
            const runningProject = this.runningProjects.get(projectId)
            if (runningProject && runningProject.shutdownHooks) {
              for (const [hookType, hook] of runningProject.shutdownHooks) {
                try {
                  runningProject.shutdownHooks.delete(hookType)
                  await hook()
                } catch (error) {
                  console.error(
                    `Error executing shutdown hook '${hookType}' for project ${projectId}:`,
                    error,
                  )
                }
              }
            }

            // Remove from running projects when it closes
            this.runningProjects.delete(projectId)
            if (!resolved) {
              reject(new Error(`Language server process exited with code ${code}.`))
            }
          })

          // Timeout if server doesn't start (skip timeout in debug mode)
          const javaToolOptions = process.env.JAVA_TOOL_OPTIONS
          const isDebugMode = javaToolOptions?.includes('jdwp')
          if (!isDebugMode) {
            setTimeout(() => {
              if (!resolved) {
                serverProcess.kill('SIGKILL')
                reject(new Error('Language server startup timeout'))
              }
            }, LANGUAGE_SERVER_STARTUP_TIMEOUT)
          }
        })
      },
    )
    this.loadingProjects.set(projectId, promise)
    promise.finally(() => this.loadingProjects.delete(projectId))
    return promise
  }

  /** Closes a project and stops its language server. */
  async closeProject(projectId: string): Promise<void> {
    // First wait for potential initialization end.
    await this.loadingProjects.get(projectId)
    const runningProject = this.runningProjects.get(projectId)

    if (!runningProject) {
      // Project is not running or already closed
      return
    }

    const { process, shutdownHooks } = runningProject

    return new Promise((resolve) => {
      // Function to execute shutdown hooks
      const executeShutdownHooks = async () => {
        for (const [hookType, hook] of shutdownHooks) {
          try {
            shutdownHooks.delete(hookType)
            await hook()
          } catch (error) {
            console.error(
              `Error executing shutdown hook '${hookType}' for project ${projectId}:`,
              error,
            )
          }
        }
      }

      // Set a timeout in case the process doesn't exit gracefully
      const timeout = setTimeout(async () => {
        if (!process.killed) {
          process.kill('SIGKILL')
        }
        await executeShutdownHooks()
        this.runningProjects.delete(projectId)
        resolve()
      }, 10000)

      // Listen for the process to exit
      process.on('exit', async () => {
        clearTimeout(timeout)
        await executeShutdownHooks()
        this.runningProjects.delete(projectId)
        resolve()
      })

      // Send line break to stdin to trigger graceful shutdown
      if (process.stdin && !process.stdin.destroyed) {
        process.stdin.write('\n')
      } else {
        process.kill('SIGTERM')
      }
    })
  }

  /** Checks if a project's language server is currently running. */
  async isProjectRunning(projectId: string): Promise<boolean> {
    return this.runningProjects.has(projectId)
  }

  /** Registers an action to be executed when the project is closed. */
  async registerShutdownHook(
    projectId: string,
    hookType: ShutdownHookType,
    hook: () => void | Promise<void>,
  ): Promise<void> {
    const runningProject = this.runningProjects.get(projectId)

    if (!runningProject) {
      // If project is not running, execute the hook immediately
      await hook()
      return
    }

    runningProject.shutdownHooks.set(hookType, hook)
  }

  /** Renames the running language server project. */
  async renameProject(
    projectId: string,
    namespace: string,
    oldPackage: string,
    newPackage: string,
  ): Promise<void> {
    const runningProject = this.runningProjects.get(projectId)
    if (!runningProject) {
      throw new Error(`Project ${projectId} is not running`)
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
  private async findServerPorts(startPort: number): Promise<[number, number, number]> {
    return new Promise((resolve, reject) => {
      portfinder.getPorts(3, { port: startPort }, (err, ports) => {
        if (err) {
          reject(new Error(`Failed to find ports: ${err}`))
        }
        if (ports.length < 3) {
          reject(new Error(`Failed to find all ports: ${ports}`))
        }
        resolve(ports as [number, number, number])
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
