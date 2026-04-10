/**
 * @file Mirrors {@link org.enso.distribution.DistributionManager} from
 * lib/scala/distribution-manager/src/main/scala/org/enso/distribution/DistributionManager.scala
 */
import { homedir } from 'node:os'
import * as path from 'node:path'

/** Compute the engine's log directory. */
export function getEngineLogDirectory(): string {
  const envLogDir = process.env.ENSO_LOG_DIRECTORY
  if (envLogDir) return envLogDir

  const home = homedir()
  switch (process.platform) {
    case 'darwin':
      return path.join(home, 'Library', 'Logs', 'org.enso')
    case 'win32': {
      const localAppData = process.env.LOCALAPPDATA ?? path.join(home, 'AppData', 'Local')
      return path.join(localAppData, 'enso', 'log')
    }
    case 'linux':
    default: {
      const xdgCache = process.env.XDG_CACHE_HOME
      if (xdgCache) return path.join(xdgCache, 'enso')
      const xdgData = process.env.XDG_DATA_HOME
      const dataDir =
        xdgData ? path.join(xdgData, 'enso') : path.join(home, '.local', 'share', 'enso')
      return path.join(dataDir, 'log')
    }
  }
}
