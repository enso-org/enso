import * as fs from 'node:fs'
import { fileURLToPath } from 'node:url'
import { defineConfig, type Plugin } from 'vite'
import defaultConfig from './vite.config'

const root = defaultConfig.root
const cacheDir = defaultConfig.cacheDir
const publicDir = defaultConfig.publicDir
const envDir = defaultConfig.envDir
const resolve = defaultConfig.resolve

export default defineConfig({
  root,
  cacheDir,
  publicDir,
  envDir,
  resolve,
  plugins: [usePolyglotFfi()],
  define: {
    ...defaultConfig.define,
    self: 'globalThis',
  },
  build: {
    minify: false, // For debugging
    emptyOutDir: true,
    outDir: '../../lib/java/ydoc-server/target/ydoc-server-bundle',
    rollupOptions: {
      input: {
        ydocServer: fileURLToPath(new URL('ydoc-server/indexPolyglot.ts', import.meta.url)),
      },
      output: {
        entryFileNames: `assets/[name].js`,
      },
    },
  },
})

/**
 * Use `ffiPolyglot` module as `ffi` interface during the build.
 */
function usePolyglotFfi(): Plugin {
  const ffiPolyglot = fileURLToPath(new URL('./shared/ast/ffiPolyglot.ts', import.meta.url))
  const ffiBackup = fileURLToPath(new URL('./shared/ast/ffiBackup.ts', import.meta.url))
  const ffi = fileURLToPath(new URL('./shared/ast/ffi.ts', import.meta.url))

  return {
    name: 'use-polyglot-ffi',
    options: () => {
      fs.renameSync(ffi, ffiBackup)
      fs.copyFileSync(ffiPolyglot, ffi)
    },
    buildEnd: () => {
      fs.renameSync(ffiBackup, ffi)
    },
  }
}
