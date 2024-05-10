/// <reference types="histoire" />

import vue from '@vitejs/plugin-vue'
import { getDefines, readEnvironmentFromFile } from 'enso-common/src/appConfig'
import { fileURLToPath } from 'node:url'
import postcssNesting from 'postcss-nesting'
import tailwindcss from 'tailwindcss'
import tailwindcssNesting from 'tailwindcss/nesting'
import { defineConfig, type Plugin } from 'vite'
// @ts-expect-error
import * as tailwindConfig from 'enso-dashboard/tailwind.config'
import { createGatewayServer } from './ydoc-server'
const localServerPort = 8080
const projectManagerUrl = 'ws://127.0.0.1:30535'

const IS_CLOUD_BUILD = process.env.CLOUD_BUILD === 'true'
const IS_POLYGLOT_YDOC_SERVER = process.env.POLYGLOT_YDOC_SERVER === 'true'

await readEnvironmentFromFile()

const entrypoint = process.env.E2E === 'true' ? './src/e2e-entrypoint.ts' : './src/entrypoint.ts'

// https://vitejs.dev/config/
export default defineConfig({
  root: fileURLToPath(new URL('.', import.meta.url)),
  cacheDir: fileURLToPath(new URL('../../node_modules/.cache/vite', import.meta.url)),
  publicDir: fileURLToPath(new URL('./public', import.meta.url)),
  envDir: fileURLToPath(new URL('.', import.meta.url)),
  plugins: [
    vue(),
    gatewayServer(),
    ...(process.env.ELECTRON_DEV_MODE === 'true' ?
      [
        (await import('@vitejs/plugin-react')).default({
          include: fileURLToPath(new URL('../ide-desktop/lib/dashboard/**/*.tsx', import.meta.url)),
          babel: { plugins: ['@babel/plugin-syntax-import-assertions'] },
        }),
      ]
    : process.env.NODE_ENV === 'development' ? [await projectManagerShim()]
    : []),
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
    ...getDefines(localServerPort),
    IS_CLOUD_BUILD: JSON.stringify(IS_CLOUD_BUILD),
    PROJECT_MANAGER_URL: JSON.stringify(projectManagerUrl),
    YDOC_SERVER_URL: IS_POLYGLOT_YDOC_SERVER ? JSON.stringify('defined') : undefined,
    RUNNING_VITEST: false,
    'import.meta.vitest': false,
    // Single hardcoded usage of `global` in aws-amplify.
    'global.TYPED_ARRAY_SUPPORT': true,
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
    rollupOptions: {
      output: {
        manualChunks: {
          fontawesome: ['@fortawesome/react-fontawesome', '@fortawesome/free-brands-svg-icons'],
        },
      },
    },
  },
})

function gatewayServer(): Plugin {
  return {
    name: 'gateway-server',
    configureServer(server) {
      if (IS_POLYGLOT_YDOC_SERVER || server.httpServer == null) return

      createGatewayServer(server.httpServer, undefined)
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
