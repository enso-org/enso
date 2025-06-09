/** @file This module contains the logic for the detection of user-specific desktop environment attributes. */

import * as childProcess from 'node:child_process'
import * as os from 'node:os'
import * as path from 'node:path'

export const DOCUMENTS = getDocumentsPath()

const CHILD_PROCESS_TIMEOUT = 3000

/** Detects path of the user documents directory depending on the operating system. */
function getDocumentsPath(): string | undefined {
  if (process.platform === 'linux') {
    return getLinuxDocumentsPath()
  } else if (process.platform === 'darwin') {
    return getMacOsDocumentsPath()
  } else if (process.platform === 'win32') {
    return getWindowsDocumentsPath()
  } else {
    return
  }
}

/** Returns the user documents path on Linux. */
function getLinuxDocumentsPath(): string {
  const xdgDocumentsPath = getXdgDocumentsPath()

  return xdgDocumentsPath ?? path.join(os.homedir(), 'enso')
}

/** Gets the documents directory from the XDG directory management system. */
function getXdgDocumentsPath(): string | undefined {
  const out = childProcess.spawnSync('xdg-user-dir', ['DOCUMENTS'], {
    timeout: CHILD_PROCESS_TIMEOUT,
  })

  if (out.error !== undefined) {
    return
  } else {
    return out.stdout.toString().trim()
  }
}

/**
 * Get the user documents path. On macOS, `Documents` acts as a symlink pointing to the
 * real locale-specific user documents directory.
 */
function getMacOsDocumentsPath(): string {
  return path.join(os.homedir(), 'Documents')
}

/** Get the path to the `My Documents` Windows directory. */
function getWindowsDocumentsPath(): string | undefined {
  const out = childProcess.spawnSync(
    'reg',
    [
      'query',
      'HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders',
      '/v',
      'personal',
    ],
    { timeout: CHILD_PROCESS_TIMEOUT },
  )

  if (out.error !== undefined) {
    return
  } else {
    const stdoutString = out.stdout.toString()
    return stdoutString.split(/\s\s+/)[4]
  }
}
