/** @file Application debug information. */

import BUILD_INFO from '../../../build.json' assert { type: 'json' }

// =================
// === Constants ===
// =================

const INDENT_SIZE = 4

// ==================
// === Debug Info ===
// ==================

/** Information about versions of different application components. */
export const VERSION_INFO = {
    version: BUILD_INFO.version,
    build: BUILD_INFO.commit,
    electron: process.versions.electron,
    chrome: process.versions.chrome,
}

/** Get the current system information, useful for debugging. */
async function getInfo() {
    const procMemInfo = await process.getProcessMemoryInfo()
    return {
        version: VERSION_INFO,
        time: {
            current: Date.now(),
            creation: process.getCreationTime(),
        },
        perf: {
            cpu: process.getCPUUsage(),
        },
        memory: {
            heap: process.getHeapStatistics(),
            blink: process.getBlinkMemoryInfo(),
            process: procMemInfo,
            system: process.getSystemMemoryInfo(),
        },
        system: {
            platform: process.platform,
            arch: process.arch,
            version: process.getSystemVersion(),
            uptime: process.uptime(),
        },
    }
}

/** Print the current system information. */
export async function printInfo() {
    const info = await getInfo()
    // This function does not accept `null` as its second parameter.
    // eslint-disable-next-line no-restricted-syntax
    console.log(JSON.stringify(info, undefined, INDENT_SIZE))
}
