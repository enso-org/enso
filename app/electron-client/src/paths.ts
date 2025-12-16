/** @file File system paths used by the application. */
import type { Electron } from '@/electron'
import { PRODUCT_NAME } from 'enso-common/src/constants'
import { homedir } from 'node:os'
import * as path from 'node:path'

/**
 * The root of the application bundle.
 *
 * This path is like:
 * - for packaged application `…/resources/app.asar`;
 * - for development `…` (just the directory with `index.js`).
 */
export function appPath(electron: Electron | undefined) {
  if (electron) {
    return electron.app.getAppPath()
  } else {
    const executableName = process.argv[0]
    if (!executableName) {
      throw new Error('Cannot determine application path: process.argv[0] is undefined.')
    }
    if (/electron/i.test(path.basename(executableName))) {
      // In development, assets are located in the CWD.
      return process.cwd()
    }
    return path.resolve(__dirname, 'resources', 'app.asar')
  }
}

/**
 * The path of the directory in which the log files of IDE are stored.
 *
 * This is based on the Electron `logs` directory, see {@link electron.app.getPath}.
 * By default, it is `~/Library/Logs/enso` on Mac, and inside `userData` directory on Windows and Linux.
 */
export function logsPath(electron: Electron | undefined) {
  if (electron) {
    return electron.app.getPath('logs')
  } else {
    const home = homedir()
    switch (process.platform) {
      case 'darwin': {
        return path.join(home, 'Library', 'Logs', PRODUCT_NAME)
      }
      case 'win32': {
        return path.join(home, 'AppData', 'Roaming', PRODUCT_NAME, 'logs')
      }
      case 'linux':
      default: {
        return path.join(home, '.config', PRODUCT_NAME, 'logs')
      }
    }
  }
}

/** The application assets, all files bundled with it. */
export function assetsPath(electron: Electron | undefined) {
  return path.join(appPath(electron), 'assets')
}

/**
 * Path to the `resources` folder.
 *
 * Contains other app resources and backend assets.
 */
export function resourcesPath(electron: Electron | undefined, electronIsDev: boolean): string {
  return electronIsDev ? appPath(electron) : path.join(appPath(electron), '..')
}

/** Relative path of Enso Project package metadata relative to the project root. */
export const PACKAGE_METADATA_RELATIVE = 'package.yaml'
/** Relative path of Enso Project PM metadata relative to the project root. */
export const PROJECT_METADATA_RELATIVE = path.join('.enso', 'project.json')

/** Path to the credentials file stored in the user's home directory. */
export const CREDENTIALS_PATH = path.join(
  homedir(),
  `.${PRODUCT_NAME.toLowerCase()}`,
  'credentials',
)
