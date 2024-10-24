/** @file Vite configuration for dashboard e2e tests' server. */
import { fileURLToPath } from 'node:url'

import { defineConfig, mergeConfig } from 'vite'

// =====================
// === Configuration ===
// =====================

// This configuration file is for dashboard tests only.
process.env.CLOUD_BUILD = 'true'
const CONFIG = (await import('./vite.config')).default

export default mergeConfig(
  CONFIG,
  defineConfig({
    mode: 'testing', // load environment from .env.testing file
    resolve: {
      alias: {
        // eslint-disable-next-line @typescript-eslint/naming-convention
        '@stripe/stripe-js/pure': fileURLToPath(
          new URL('./e2e/dashboard/mock/stripe.ts', import.meta.url),
        ),
        // eslint-disable-next-line @typescript-eslint/naming-convention
        '@stripe/react-stripe-js': fileURLToPath(
          new URL('./e2e/dashboard/mock/react-stripe.tsx', import.meta.url),
        ),
      },
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
    build: {
      outDir: fileURLToPath(new URL('./mockDist', import.meta.url)),
    },
  }),
)
