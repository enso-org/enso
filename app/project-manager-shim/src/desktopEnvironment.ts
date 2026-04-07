/** @file This module contains the logic for the detection of user-specific desktop environment attributes. */

import * as childProcess from 'node:child_process'
import * as os from 'node:os'
import * as path from 'node:path'

const CHILD_PROCESS_TIMEOUT = 3000
const PATH_DIAGNOSTICS_LABEL = 'PATH_DIAGNOSTICS'

export const DOCUMENTS = getDocumentsPath()

function formatStringDiagnostics(value: string): string {
  const codePoints = Array.from(
    value,
    (char) => `U+${char.codePointAt(0)?.toString(16).toUpperCase().padStart(4, '0')}`,
  )
  return JSON.stringify({
    value,
    length: value.length,
    codePoints,
  })
}

function formatBufferDiagnostics(value: Buffer): string {
  return JSON.stringify({
    length: value.length,
    hex: value.toString('hex'),
  })
}

function pathDiagnostics(
  label: string,
  details: Record<string, string | Buffer | undefined>,
): void {
  const formattedDetails = Object.fromEntries(
    Object.entries(details).map(([key, value]) => {
      if (value == null) {
        return [key, null]
      } else if (typeof value === 'string') {
        return [key, JSON.parse(formatStringDiagnostics(value))]
      } else {
        return [key, JSON.parse(formatBufferDiagnostics(value))]
      }
    }),
  )
  console.warn(`[${PATH_DIAGNOSTICS_LABEL}] ${label}: ${JSON.stringify(formattedDetails)}`)
}

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
function getWindowsDocumentsPath() {
  const out = childProcess.spawnSync(
    'powershell',
    [
      '[System.Convert]::ToBase64String([System.Text.Encoding]::UTF8.GetBytes([Environment]::GetFolderPath("MyDocuments")))',
    ],
    { timeout: CHILD_PROCESS_TIMEOUT },
  )
  pathDiagnostics('getWindowsDocumentsPath.spawnSync', {
    stdoutBuffer: out.stdout,
    stderrBuffer: out.stderr,
    stdoutUtf8: out.stdout.toString('utf8'),
  })

  if (out.error) {
    console.warn(`[${PATH_DIAGNOSTICS_LABEL}] getWindowsDocumentsPath.error: ${out.error.message}`)
    return undefined
  }

  const resultBase64 = out.stdout.toString().trim() || undefined
  const result = resultBase64 ? Buffer.from(resultBase64, 'base64').toString('utf8') : undefined
  pathDiagnostics('getWindowsDocumentsPath.result', {
    result,
  })
  return result
}
