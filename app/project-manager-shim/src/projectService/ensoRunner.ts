export interface EnsoRunner {
  createProject(
    path: string,
    name: string,
    engineVersion?: string,
    projectTemplate?: string,
  ): Promise<void>
}

import * as fs from 'node:fs/promises'
import * as path from 'node:path'

/** Find the path to the `enso` executable. */
export async function ensoPath(baseDirectory: string): Promise<string | undefined> {
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
  const resourcesDir = path.join(baseDirectory, 'resources', 'enso', 'dist')
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
  const builtDistDir = path.join(baseDirectory, 'built-distribution')
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
