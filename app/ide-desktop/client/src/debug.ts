/** @file Application debug information. */

import * as buildUtils from 'enso-common/src/buildUtils'

import BUILD_INFO from '../buildInfo'

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
  console.log(JSON.stringify(info, undefined, buildUtils.INDENT_SIZE))
}
