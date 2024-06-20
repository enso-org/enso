import * as child_process from 'node:child_process'
import * as os from 'node:os'
import * as path from 'node:path'

import * as detect from 'enso-common/src/detect'

/**
 * Detects path of the user documents directory depending on the operating system.
 */
export function getDocumentsPath(): string | undefined {
    if (detect.isOnLinux()) {
        return getLinuxDocumentsPath()
    }
    if (detect.isOnMacOS()) {
        return getMacOsDocumentsPath()
    }
    if (detect.isOnWindows()) {
        return getWindowsDocumentsPath()
    }
}

/**
 * Returns the user documents path on Linux.
 */
function getLinuxDocumentsPath(): string {
    const xdgDocumentsPath = getXdgDocumentsPath()
    if (xdgDocumentsPath == null) {
        return path.join(os.homedir(), 'enso')
    }

    return xdgDocumentsPath
}

/**
 * Gets the documents directory from the XDG directory management system.
 */
function getXdgDocumentsPath(): string | undefined {
    const out = child_process.spawnSync('xdg-user-dir', ['DOCUMENTS'], { timeout: 3000 })
    if (out.error) {
        return
    }

    return out.stdout.toString('utf8').trim()
}

/**
 * Get the user documents path. On macOS, `Documents` acts as a symlink pointing to the
 * real locale-specific user documents directory.
 */
function getMacOsDocumentsPath(): string {
    return path.join(os.homedir(), 'Documents')
}

/**
 * Get the path to the `My Documents` Windows directory.
 */
function getWindowsDocumentsPath(): string | undefined {
    const out = child_process.spawnSync(
        'reg',
        [
            'query',
            '"HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\ShellFolders"',
            '/v',
            'personal',
        ],
        { timeout: 3000 }
    )

    if (out.error) {
        return
    }

    const stdoutString = out.stdout.toString('utf8')

    return stdoutString.split('\\s\\s+')[4]
}
