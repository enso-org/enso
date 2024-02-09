/** @file Configuration for vite. */
import * as vite from 'vite'

import config from './vite.config'

// =====================
// === Configuration ===
// =====================

/* eslint-disable @typescript-eslint/naming-convention */

export default vite.mergeConfig(
  config,
  vite.defineConfig({
    define: {
      'process.env.SUPPORTS_LOCAL_BACKEND': JSON.stringify('true'),
    },
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
