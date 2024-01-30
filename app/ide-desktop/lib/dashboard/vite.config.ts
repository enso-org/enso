/** @file Configuration for vite. */
import * as url from 'node:url'

import vitePluginYaml from '@modyfi/vite-plugin-yaml'
import vitePluginReact from '@vitejs/plugin-react'
import * as vite from 'vite'

import * as globals from './globals'

// =====================
// === Configuration ===
// =====================

/* eslint-disable @typescript-eslint/naming-convention */
export default vite.defineConfig(env => {
  const { mode } = env
  const serverPort = 8080
  const devMode = mode === 'development'
  return {
    server: { port: serverPort },
    plugins: [vitePluginReact({ include: '**/*.tsx' }), vitePluginYaml()],
    resolve: {
      alias: {
        '#': url.fileURLToPath(new URL('./src', import.meta.url)),
      },
    },
    define: {
      IS_VITE: JSON.stringify(true),
      // The sole hardcoded usage of `global` in by aws-amplify.
      'global.TYPED_ARRAY_SUPPORT': JSON.stringify(true),
      ...globals.globals(devMode, serverPort),
    },
  }
})
