/** @file Configuration for vite. */
import * as url from 'node:url'

import vitePluginYaml from '@modyfi/vite-plugin-yaml'
import vitePluginReact from '@vitejs/plugin-react'
import * as vite from 'vite'

// =================
// === Constants ===
// =================

const SERVER_PORT = 8080

// =====================
// === Configuration ===
// =====================

/* eslint-disable @typescript-eslint/naming-convention */

export default vite.defineConfig({
  server: { port: SERVER_PORT },
  plugins: [vitePluginReact({ include: '**/*.tsx' }), vitePluginYaml()],
  resolve: {
    alias: {
      '#': url.fileURLToPath(new URL('./src', import.meta.url)),
    },
  },
  define: {
    IS_VITE: JSON.stringify(true),
    REDIRECT_OVERRIDE: JSON.stringify(`http://localhost:${SERVER_PORT}`),
    CLOUD_ENV:
      process.env.ENSO_CLOUD_ENV != null ? JSON.stringify(process.env.ENSO_CLOUD_ENV) : 'undefined',
    // Single hardcoded usage of `global` in by aws-amplify.
    'global.TYPED_ARRAY_SUPPORT': JSON.stringify(true),
  },
})
