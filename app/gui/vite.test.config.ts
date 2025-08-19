/** @file Vite configuration for dashboard integration tests' server. */
import { fileURLToPath } from 'node:url'
import { defineConfig, mergeConfig } from 'vite'

// This configuration file is for dashboard tests only.
process.env.DASHBOARD_TESTS = 'true'
const CONFIG = (await import('./vite.config')).default

export default mergeConfig(
  CONFIG,
  defineConfig({
    mode: 'testing', // load environment from .env.testing file
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
    build: {
      outDir: fileURLToPath(new URL('./mockDist', import.meta.url)),
    },
  }),
)
