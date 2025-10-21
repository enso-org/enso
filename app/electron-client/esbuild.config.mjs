/** @file Esbuild config for building the electron client. */
import { fileURLToPath } from 'node:url'

import { wasmLoader } from 'esbuild-plugin-wasm'

export default {
  outExtension: { '.js': '.mjs' },
  plugins: [wasmLoader()],
  alias: {
    '@': fileURLToPath(new URL('./src', import.meta.url)),
  },
}
