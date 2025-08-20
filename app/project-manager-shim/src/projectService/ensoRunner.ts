import * as child_process from 'node:child_process'
import * as fs from 'node:fs/promises'
import * as path from 'node:path'

export interface Runner {
  createProject(
    path: string,
    name: string,
    engineVersion?: string,
    projectTemplate?: string,
  ): Promise<void>
}

export class EnsoRunner implements Runner {
  constructor(private ensoPath: string) {}

  async createProject(
    projectPath: string,
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
      const process = child_process.spawn(this.ensoPath, args)

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
          reject(new Error(`Enso process exited with code ${code}. stderr: ${stderr}`))
        }
      })
    })
  }
}

/** Find the path to the `enso` executable. */
export async function findEnsoPath(workDir: string): Promise<string | undefined> {
  const checkExecutable = async (filePath: string) => {
    try {
      await fs.access(filePath, fs.constants.X_OK)
    } catch {
      throw new Error(`Enso executable at ${filePath} is not executable`)
    }
    return filePath
  }

  // Check ENSO_RUNNER_PATH environment variable first
  const envPath = process.env.ENSO_RUNNER_PATH
  if (envPath) {
    try {
      await fs.access(envPath)
      return await checkExecutable(envPath)
    } catch {
      // File doesn't exist, continue searching
    }
  }

  // Check resources/enso/dist/*/bin/enso
  const resourcesDir = path.join(workDir, 'resources', 'enso', 'dist')
  try {
    const stat = await fs.stat(resourcesDir)
    if (stat.isDirectory()) {
      const distDirs = await fs.readdir(resourcesDir)
      for (const distDir of distDirs) {
        const ensoPath = path.join(resourcesDir, distDir, 'bin', 'enso')
        try {
          await fs.access(ensoPath)
          return await checkExecutable(ensoPath)
        } catch {
          // File doesn't exist, continue searching
        }
      }
    }
  } catch {
    // Directory doesn't exist, continue to next check
  }

  // Check built-distribution/*/*/bin/enso
  const builtDistDir = path.join(workDir, 'built-distribution')
  try {
    const stat = await fs.stat(builtDistDir)
    if (stat.isDirectory()) {
      const topLevelDirs = await fs.readdir(builtDistDir)
      for (const topDir of topLevelDirs) {
        const topPath = path.join(builtDistDir, topDir)
        const topStat = await fs.stat(topPath)
        if (topStat.isDirectory()) {
          const subDirs = await fs.readdir(topPath)
          for (const subDir of subDirs) {
            const ensoPath = path.join(topPath, subDir, 'bin', 'enso')
            try {
              await fs.access(ensoPath)
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
