/// <reference types="histoire" />

import vue from '@vitejs/plugin-vue'
import { fileURLToPath } from 'node:url'
import postcssNesting from 'postcss-nesting'
import tailwindcss from 'tailwindcss'
import tailwindcssNesting from 'tailwindcss/nesting'
import { defineConfig, type Plugin } from 'vite'
import topLevelAwait from 'vite-plugin-top-level-await'
import { globals } from '../ide-desktop/lib/dashboard/globals'
// @ts-expect-error
import * as tailwindConfig from '../ide-desktop/lib/dashboard/tailwind.config'
import { createGatewayServer } from './ydoc-server'
const localServerPort = 8080
const projectManagerUrl = 'ws://127.0.0.1:30535'

const IS_CLOUD_BUILD = process.env.CLOUD_BUILD === 'true'

// https://vitejs.dev/config/
export default defineConfig({
  cacheDir: '../../node_modules/.cache/vite',
  plugins: [vue(), gatewayServer(), topLevelAwait()],
  optimizeDeps: {
    entries: 'index.html',
  },
  server: {
    headers: {
      'Cross-Origin-Embedder-Policy': 'require-corp',
      'Cross-Origin-Opener-Policy': 'same-origin',
      'Cross-Origin-Resource-Policy': 'same-origin',
    },
  },
  resolve: {
    alias: {
      ...(process.env.E2E === 'true'
        ? { '/src/main.ts': fileURLToPath(new URL('./e2e/main.ts', import.meta.url)) }
        : {}),
      shared: fileURLToPath(new URL('./shared', import.meta.url)),
      'rust-ffi': fileURLToPath(new URL('./rust-ffi', import.meta.url)),
      '@': fileURLToPath(new URL('./src', import.meta.url)),
    },
  },
  define: {
    ...globals(!IS_CLOUD_BUILD, localServerPort),
    IS_CLOUD_BUILD: JSON.stringify(IS_CLOUD_BUILD),
    PROJECT_MANAGER_URL: JSON.stringify(projectManagerUrl),
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
            glob.replace(/^[.][/]/, '../ide-desktop/lib/dashboard/'),
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
          'aws-amplify': ['@aws-amplify/core', '@aws-amplify/auth'],
        },
      },
    },
  },
})

function gatewayServer(): Plugin {
  return {
    name: 'gateway-server',
    configureServer(server) {
      if (server.httpServer == null) return

      createGatewayServer(server.httpServer)
    },
  }
}
