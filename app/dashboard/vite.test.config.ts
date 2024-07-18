/** @file Configuration for vite. */
import * as vite from 'vite'

import * as appConfig from 'enso-common/src/appConfig'

// =====================
// === Configuration ===
// =====================

appConfig.loadTestEnvironmentVariables()

const CONFIG = (await import('./vite.config')).default

export default vite.mergeConfig(
  CONFIG,
  vite.defineConfig({
    resolve: {
      extensions: [
        '.mock.mjs',
        '.mock.js',
        '.mock.mts',
        '.mock.ts',
        '.mock.jsx',
        '.mock.tsx',
        '.mock.json',
        '.mjs',
        '.js',
        '.mts',
        '.ts',
        '.jsx',
        '.tsx',
        '.json',
      ],
    },
    define: {
      // eslint-disable-next-line @typescript-eslint/naming-convention
      'process.env.IS_IN_PLAYWRIGHT_TEST': JSON.stringify(`${true}`),
    },
  })
)
