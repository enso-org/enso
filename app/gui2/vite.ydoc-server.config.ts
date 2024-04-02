/// <reference types="histoire" />

import { getDefines, readEnvironmentFromFile } from 'enso-common/src/appConfig'
import { fileURLToPath } from 'node:url'
import { defineConfig } from 'vite'
const localServerPort = 8080
const projectManagerUrl = 'ws://127.0.0.1:30535'

const IS_CLOUD_BUILD = process.env.CLOUD_BUILD === 'true'

await readEnvironmentFromFile()

// https://vitejs.dev/config/
export default defineConfig({
  root: fileURLToPath(new URL('.', import.meta.url)),
  cacheDir: fileURLToPath(new URL('../../node_modules/.cache/vite', import.meta.url)),
  publicDir: fileURLToPath(new URL('./public', import.meta.url)),
  envDir: fileURLToPath(new URL('.', import.meta.url)),
  resolve: {
    alias: {
      shared: fileURLToPath(new URL('./shared', import.meta.url)),
      '@': fileURLToPath(new URL('./src', import.meta.url)),
    },
  },
  define: {
    ...getDefines(localServerPort),
    IS_CLOUD_BUILD: JSON.stringify(IS_CLOUD_BUILD),
    PROJECT_MANAGER_URL: JSON.stringify(projectManagerUrl),
    RUNNING_VITEST: false,
    'import.meta.vitest': false,
    // Single hardcoded usage of `global` in aws-amplify.
    'global.TYPED_ARRAY_SUPPORT': true,
  },
  build: {
    minify: false, // For debugging
    emptyOutDir: true,
    outDir: '../../lib/java/polyglot-ydoc-server/target/classes/dist',
    /* lib: {
      entry: fileURLToPath(new URL('ydoc-server/server.ts', import.meta.url)),
      name: 'ydocServer',
      fileName: 'ydocServer'
    }, */
    rollupOptions: {
      input: {
        ydocServer: fileURLToPath(new URL('ydoc-server/server.ts', import.meta.url)),
      },
      output: {
        entryFileNames: `assets/[name].js`,
      },
    },
  },
})
