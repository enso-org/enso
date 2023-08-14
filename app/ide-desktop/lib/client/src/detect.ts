/** @file Feature detection. */
import * as childProcess from 'node:child_process'
import * as os from 'node:os'

// ========================
// === supportsVibrancy ===
// ========================

const WINDOWS_VIBRANCY_MINIMUM_MAJOR_VERSION = 10
const WINDOWS_VIBRANCY_MINIMUM_BUILD = 22523

/** Whether the current operating system supports having a translucent blurred background. */
export function supportsVibrancy() {
    switch (os.platform()) {
        case 'darwin': {
            return true
        }
        case 'win32': {
            const match = childProcess
                .execSync('ver')
                .toString()
                .match(/(?<major>\d+)\.(?<minor>\d+)\.(?<build>\d+)/)
            if (match == null) {
                return false
            } else {
                const { major, build } = match.groups ?? {}
                return (
                    Number(major) >= WINDOWS_VIBRANCY_MINIMUM_MAJOR_VERSION &&
                    Number(build) >= WINDOWS_VIBRANCY_MINIMUM_BUILD
                )
            }
        }
        default: {
            return false
        }
    }
}
