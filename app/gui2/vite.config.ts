/// <reference types="histoire" />

import vue from '@vitejs/plugin-vue'
import { fileURLToPath } from 'node:url'
import postcssNesting from 'postcss-nesting'
import tailwindcss from 'tailwindcss'
import tailwindcssNesting from 'tailwindcss/nesting'
import { defineConfig, type Plugin } from 'vite'
import topLevelAwait from 'vite-plugin-top-level-await'
import * as tailwindConfig from '../ide-desktop/lib/dashboard/tailwind.config'
import { createGatewayServer } from './ydoc-server'
const localServerPort = 8080
const projectManagerUrl = 'ws://127.0.0.1:30535'

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
    REDIRECT_OVERRIDE: JSON.stringify(`http://localhost:${localServerPort}`),
    PROJECT_MANAGER_URL: JSON.stringify(projectManagerUrl),
    IS_DEV_MODE: JSON.stringify(process.env.NODE_ENV !== 'production'),
    CLOUD_ENV:
      process.env.ENSO_CLOUD_ENV != null ? JSON.stringify(process.env.ENSO_CLOUD_ENV) : 'undefined',
    RUNNING_VITEST: false,
    'import.meta.vitest': false,
    // Single hardcoded usage of `global` in by aws-amplify.
    'global.TYPED_ARRAY_SUPPORT': true,
  },
  assetsInclude: ['**/*.yaml', '**/*.svg'],
  css: {
    postcss: {
      plugins: [tailwindcssNesting(postcssNesting()), tailwindcss({ config: tailwindConfig })],
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
