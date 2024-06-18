/** @file Configuration for vite. */
import * as vite from 'vite'

import * as appConfig from 'enso-common/src/appConfig'

// =====================
// === Configuration ===
// =====================

/* eslint-disable @typescript-eslint/naming-convention */

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
  })
)
