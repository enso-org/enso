/** @file Esbuild config file. */
import { fileURLToPath } from 'node:url'

import { wasmLoader } from 'esbuild-plugin-wasm'

export default {
  format: 'esm',
  platform: 'node',
  outExtension: { '.js': '.mjs' },
  plugins: [wasmLoader()],
  target: ['node20'], // electron31
  alias: {
    '@': fileURLToPath(new URL('./src', import.meta.url)),
  },
  define: {
    'process.env.ELECTRON_DEV_MODE': 'false',
    'process.env.GUI_CONFIG_PATH': '"../gui/vite.config.ts"',
    'process.env.ENSO_IDE_VERSION': '"2025.0.0-dev"',
    'process.env.ENSO_IDE_COMMIT_HASH': '"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"',
  },
}
