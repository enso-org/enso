/** @file Application debug information. */

import buildCfg from '../../../build.json'

// ==================
// === Debug Info ===
// ==================

/** Information about versions of different application components. */
export const versionInfo = {
    version: buildCfg.version,
    build: buildCfg.commit,
    electron: process.versions.electron,
    chrome: process.versions.chrome,
}

async function getInfo() {
    let procMemInfo = await process.getProcessMemoryInfo()
    return {
        version: versionInfo,
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

export async function printInfoAndExit() {
    let info = await getInfo()
    console.log(JSON.stringify(info, undefined, 4))
    process.exit()
}
