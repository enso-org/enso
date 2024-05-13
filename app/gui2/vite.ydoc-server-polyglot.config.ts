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
  resolve: {
    ...resolve,
    alias: {
      ...resolve?.alias,
      // Use `ffiPolyglot` module as `ffi` interface during the build.
      'shared/ast/ffi': fileURLToPath(new URL('./shared/ast/ffiPolyglot.ts', import.meta.url)),
    },
  },
  define: {
    ...defaultConfig.define,
    self: 'globalThis',
  },
  build: {
    minify: false, // For debugging
    emptyOutDir: true,
    outDir: '../../lib/java/ydoc-server/target/classes/dist',
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
