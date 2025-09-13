import { Path } from 'enso-common/src/utilities/file'
import * as childProcess from 'node:child_process'
import * as fs from 'node:fs'
import { createWriteStream } from 'node:fs'
import * as os from 'node:os'
import * as path from 'node:path'
import { pipeline } from 'node:stream/promises'
import { extract } from 'tar'

export interface Runner {
  createProject(
    path: Path,
    name: string,
    engineVersion?: string,
    projectTemplate?: string,
  ): Promise<void>
}

/** Implementation of Runner that uses the Enso executable. */
export class EnsoRunner implements Runner {
  /** Creates a new EnsoRunner with the path to the Enso executable. */
  constructor(private ensoPath: Path) {}

  /** Creates a new Enso project at the specified path. */
  async createProject(
    projectPath: Path,
    name: string,
    engineVersion?: string,
    projectTemplate?: string,
  ): Promise<void> {
    if (!this.ensoPath) {
      throw new Error('Enso executable not found')
    }

    const args: string[] = []
    args.push('--new', projectPath)
    args.push('--new-project-name', name)
    if (projectTemplate) {
      args.push('--new-project-template', projectTemplate)
    }

    return new Promise((resolve, reject) => {
      const process = childProcess.spawn(this.ensoPath, args)

      let _stdout = ''
      let stderr = ''

      process.stdout.on('data', (data) => {
        _stdout += data.toString()
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
          reject(new Error(`Enso process exited with code ${code}. stderr: ${stderr}`))
        }
      })
    })
  }
}

/** Find the path to the `enso` executable. */
export function findEnsoExecutable(workDir: string = '.'): Path | undefined {
  const checkExecutable = (filePath: string) => {
    try {
      fs.accessSync(filePath, fs.constants.X_OK)
    } catch {
      throw new Error(`Enso executable at ${filePath} is not executable`)
    }
    return Path(filePath)
  }

  let ensoExecutable: string
  if (os.platform() === 'win32') {
    ensoExecutable = 'enso.exe'
  } else {
    ensoExecutable = 'enso'
  }

  // Check ENSO_RUNNER_PATH environment variable first
  const envPath = process.env.ENSO_RUNNER_PATH
  if (envPath) {
    try {
      fs.accessSync(envPath)
      return checkExecutable(envPath)
    } catch {
      // File doesn't exist, continue searching
    }
  }

  // Check enso/dist/*/bin/enso
  const ensoDistPath = path.join(workDir, 'enso', 'dist')
  try {
    const stat = fs.statSync(ensoDistPath)
    if (stat.isDirectory()) {
      const distDirs = fs.readdirSync(ensoDistPath)
      for (const distDir of distDirs) {
        const ensoPath = path.join(ensoDistPath, distDir, 'bin', ensoExecutable)
        try {
          fs.accessSync(ensoPath)
          return checkExecutable(ensoPath)
        } catch {
          // File doesn't exist, continue searching
        }
      }
    }
  } catch {
    // Directory doesn't exist, continue to next directory
  }

  // Check built-distribution/*/*/bin/enso
  const builtDistDir = path.join(workDir, 'built-distribution')
  try {
    const stat = fs.statSync(builtDistDir)
    if (stat.isDirectory()) {
      const topLevelDirs = fs.readdirSync(builtDistDir)
      for (const topDir of topLevelDirs) {
        const topPath = path.join(builtDistDir, topDir)
        const topStat = fs.statSync(topPath)
        if (topStat.isDirectory()) {
          const subDirs = fs.readdirSync(topPath)
          for (const subDir of subDirs) {
            const ensoPath = path.join(topPath, subDir, 'bin', ensoExecutable)
            try {
              fs.accessSync(ensoPath)
              return checkExecutable(ensoPath)
            } catch {
              // File doesn't exist, continue searching
            }
          }
        }
      }
    }
  } catch {
    // Directory doesn't exist
  }

  // No enso executable found
  return undefined
}

/**
 * Downloads the latest Enso engine prerelease from GitHub.
 *
 * This function automatically detects the current platform (macOS, Linux, or Windows)
 * and architecture (amd64 or aarch64) to download the appropriate engine binary.
 * The engine is downloaded from the latest GitHub prerelease and extracted to
 * the built-distribution directory.
 * @param projectRoot - The root directory of the project where the engine will be installed
 * @returns A promise that resolves to the path where the engine was extracted
 */
export async function downloadEnsoEngine(projectRoot: string): Promise<string> {
  console.log('Downloading latest Enso engine...')

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

  // Fetch all releases from GitHub API and find the latest prerelease
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

  // Find the latest prerelease with the matching asset
  const prereleases = releases.filter((release: any) => release.prerelease)

  if (prereleases.length === 0) {
    throw new Error('No prereleases found')
  }

  let releaseData: any = null
  let asset: any = null
  let assetName: string = ''

  // Iterate through prereleases to find one with matching asset
  for (const prerelease of prereleases) {
    const version = prerelease.tag_name
    assetName = `enso-engine-${version}-${platformString}-${archString}${extensionString}`
    asset = prerelease.assets.find((a: any) => a.name === assetName)

    if (asset) {
      releaseData = prerelease
      break
    }
  }

  if (!releaseData || !asset) {
    throw new Error(
      `Could not find asset: enso-engine-*-${platformString}-${archString}${extensionString} in any prerelease`,
    )
  }

  console.log(`Downloading ${assetName}...`)

  // Download the asset
  const downloadResponse = await fetch(asset.browser_download_url, { headers })

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
    await pipeline(
      fs.createReadStream(archivePath),
      extract({
        cwd: extractDir,
      }),
    )
  } else {
    await new Promise<void>((resolve, reject) => {
      const unzipProcess = childProcess.spawn('unzip', ['-o', archivePath, '-d', extractDir], {
        stdio: 'ignore',
      })

      unzipProcess.on('error', (error) => {
        reject(new Error(`Failed to extract zip: ${error.message}`))
      })

      unzipProcess.on('close', (code) => {
        if (code === 0) {
          resolve()
        } else {
          reject(new Error(`unzip process exited with code ${code}`))
        }
      })
    })
  }

  // Clean up the archive file
  fs.unlinkSync(archivePath)

  console.log(`Enso engine downloaded and extracted to ${extractDir}`)

  return extractDir
}
