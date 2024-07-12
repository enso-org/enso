/// <reference types="histoire" />

import react from '@vitejs/plugin-react'
import vue from '@vitejs/plugin-vue'
import { getDefines, readEnvironmentFromFile } from 'enso-common/src/appConfig'
import { fileURLToPath } from 'node:url'
import postcssNesting from 'postcss-nesting'
import tailwindcss from 'tailwindcss'
import tailwindcssNesting from 'tailwindcss/nesting'
import { defineConfig, type Plugin } from 'vite'
import VueDevTools from 'vite-plugin-vue-devtools'
// @ts-expect-error
import * as tailwindConfig from 'enso-dashboard/tailwind.config'
import { createGatewayServer } from './ydoc-server'
const projectManagerUrl = 'ws://127.0.0.1:30535'

const IS_CLOUD_BUILD = process.env.CLOUD_BUILD === 'true'
const POLYGLOT_YDOC_SERVER = process.env.POLYGLOT_YDOC_SERVER

await readEnvironmentFromFile()

const entrypoint = process.env.E2E === 'true' ? './src/e2e-entrypoint.ts' : './src/entrypoint.ts'

// https://vitejs.dev/config/
export default defineConfig({
  root: fileURLToPath(new URL('.', import.meta.url)),
  cacheDir: fileURLToPath(new URL('../../node_modules/.cache/vite', import.meta.url)),
  publicDir: fileURLToPath(new URL('./public', import.meta.url)),
  envDir: fileURLToPath(new URL('.', import.meta.url)),
  plugins: [
    VueDevTools(),
    vue(),
    react({
      include: fileURLToPath(new URL('../ide-desktop/lib/dashboard/**/*.tsx', import.meta.url)),
      babel: { plugins: ['@babel/plugin-syntax-import-attributes'] },
    }),
    gatewayServer(),
    ...(process.env.NODE_ENV === 'development' ? [await projectManagerShim()] : []),
  ],
  optimizeDeps: {
    entries: fileURLToPath(new URL('./index.html', import.meta.url)),
  },
  server: {
    headers: {
      'Cross-Origin-Opener-Policy': 'same-origin',
      'Cross-Origin-Resource-Policy': 'same-origin',
    },
  },
  resolve: {
    alias: {
      '/src/entrypoint.ts': fileURLToPath(new URL(entrypoint, import.meta.url)),
      shared: fileURLToPath(new URL('./shared', import.meta.url)),
      '@': fileURLToPath(new URL('./src', import.meta.url)),
    },
  },
  define: {
    ...getDefines(),
    IS_CLOUD_BUILD: JSON.stringify(IS_CLOUD_BUILD),
    PROJECT_MANAGER_URL: JSON.stringify(projectManagerUrl),
    YDOC_SERVER_URL: JSON.stringify(POLYGLOT_YDOC_SERVER),
    RUNNING_VITEST: false,
    'import.meta.vitest': false,
    // Single hardcoded usage of `global` in aws-amplify.
    'global.TYPED_ARRAY_SUPPORT': true,
  },
  esbuild: {
    dropLabels: process.env.NODE_ENV === 'development' ? [] : ['DEV'],
  },
  assetsInclude: ['**/*.yaml', '**/*.svg'],
  css: {
    postcss: {
      plugins: [
        tailwindcssNesting(postcssNesting()),
        tailwindcss({
          ...tailwindConfig.default,
          content: tailwindConfig.default.content.map((glob: string) =>
            glob.replace(
              /^[.][/]/,
              fileURLToPath(new URL('../ide-desktop/lib/dashboard/', import.meta.url)),
            ),
          ),
        }),
      ],
    },
  },
  build: {
    // dashboard chunk size is larger than the default warning limit
    chunkSizeWarningLimit: 700,
  },
})

function gatewayServer(): Plugin {
  return {
    name: 'gateway-server',
    configureServer({ httpServer }) {
      if (httpServer == null || POLYGLOT_YDOC_SERVER != undefined) return
      createGatewayServer(httpServer, undefined)
    },
  }
}

async function projectManagerShim(): Promise<Plugin> {
  const module = await import(
    '../ide-desktop/lib/project-manager-shim/src/projectManagerShimMiddleware'
  )
  return {
    name: 'project-manager-shim',
    configureServer(server) {
      server.middlewares.use(module.default)
    },
  }
}
