import * as childProcess from 'node:child_process'
import * as fs from 'node:fs'
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
      const process = childProcess.spawn(this.ensoPath, args)

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
export function findEnsoPath(workDir: string): string | undefined {
  const checkExecutable = (filePath: string) => {
    try {
      fs.accessSync(filePath, fs.constants.X_OK)
    } catch {
      throw new Error(`Enso executable at ${filePath} is not executable`)
    }
    return filePath
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
        const ensoPath = path.join(ensoDistPath, distDir, 'bin', 'enso')
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
            const ensoPath = path.join(topPath, subDir, 'bin', 'enso')
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
