/** @file Configuration for vite. */
import * as url from 'node:url'

import vitePluginYaml from '@modyfi/vite-plugin-yaml'
import vitePluginReact from '@vitejs/plugin-react'
import * as vite from 'vite'

import * as common from 'enso-common'

import * as appConfig from 'enso-common/src/appConfig'

// =====================
// === Configuration ===
// =====================

const SERVER_PORT = 8080
await appConfig.readEnvironmentFromFile()

/* eslint-disable @typescript-eslint/naming-convention */
export default vite.defineConfig({
  server: { port: SERVER_PORT, headers: Object.fromEntries(common.COOP_COEP_CORP_HEADERS) },
  plugins: [
    vitePluginReact({
      include: '**/*.tsx',
      babel: { plugins: ['@babel/plugin-syntax-import-assertions'] },
    }),
    vitePluginYaml(),
  ],
  resolve: {
    alias: {
      '#': url.fileURLToPath(new URL('./src', import.meta.url)),
    },
  },
  build: {
    rollupOptions: {
      input: {
        main: url.fileURLToPath(new URL('./index.html', import.meta.url)),
        '404': url.fileURLToPath(new URL('./404.html', import.meta.url)),
      },
    },
  },
  define: {
    IS_VITE: JSON.stringify(true),
    // The sole hardcoded usage of `global` in aws-amplify.
    'global.TYPED_ARRAY_SUPPORT': JSON.stringify(true),
    ...appConfig.getDefines(SERVER_PORT),
  },
})
