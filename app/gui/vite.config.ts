import react from '@vitejs/plugin-react'
import vue from '@vitejs/plugin-vue'
import { COOP_COEP_CORP_HEADERS } from 'enso-common'
import { fileURLToPath } from 'node:url'
import postcssNesting from 'postcss-nesting'
import tailwindcss from 'tailwindcss'
import tailwindcssNesting from 'tailwindcss/nesting'
import { defineConfig, type Plugin } from 'vite'
import VueDevTools from 'vite-plugin-vue-devtools'
import wasm from 'vite-plugin-wasm'
import tailwindConfig from './tailwind.config'

const isDevMode = process.env.NODE_ENV === 'development'
const isE2E = process.env.E2E === 'true'

const entrypoint = isE2E ? './src/project-view/e2e-entrypoint.ts' : './src/entrypoint.ts'

process.env.ENSO_IDE_YDOC_SERVER_URL ||= isDevMode ? 'ws://__HOSTNAME__:59776' : undefined
process.env.ENSO_IDE_PROJECT_MANAGER_URL ||= isDevMode ? 'ws://__HOSTNAME__:30535' : undefined

// https://vitejs.dev/config/
export default defineConfig({
  cacheDir: fileURLToPath(new URL('../../node_modules/.cache/vite', import.meta.url)),
  plugins: [
    wasm(),
    ...(isDevMode ?
      [
        await VueDevTools(),
        react({
          include: fileURLToPath(new URL('../dashboard/**/*.tsx', import.meta.url)),
          babel: { plugins: ['@babel/plugin-syntax-import-attributes'] },
        }),
      ]
    : []),
    vue({
      customElement: ['**/components/visualizations/**', '**/components/shared/**'],
      template: {
        compilerOptions: {
          isCustomElement: (tag) => tag.startsWith('enso-'),
        },
      },
    }),
    react({
      include: fileURLToPath(new URL('./src/dashboard/**/*.tsx', import.meta.url)),
      babel: { plugins: ['@babel/plugin-syntax-import-attributes'] },
    }),
    ...(isDevMode ? [await projectManagerShim()] : []),
  ],
  optimizeDeps: {
    entries: fileURLToPath(new URL('./index.html', import.meta.url)),
  },
  server: {
    headers: Object.fromEntries(COOP_COEP_CORP_HEADERS),
    ...(process.env.GUI_HOSTNAME ? { host: process.env.GUI_HOSTNAME } : {}),
  },
  resolve: {
    conditions: isDevMode ? ['source'] : [],
    alias: {
      '/src/entrypoint.ts': fileURLToPath(new URL(entrypoint, import.meta.url)),
      shared: fileURLToPath(new URL('./shared', import.meta.url)),
      '@': fileURLToPath(new URL('./src/project-view', import.meta.url)),
      '#': fileURLToPath(new URL('./src/dashboard', import.meta.url)),
    },
  },
  envPrefix: 'ENSO_IDE_',
  define: {
    // Single hardcoded usage of `global` in aws-amplify.
    'global.TYPED_ARRAY_SUPPORT': true,
  },
  esbuild: {
    dropLabels: isDevMode ? [] : ['DEV'],
    supported: {
      'top-level-await': true,
    },
  },
  assetsInclude: ['**/*.svg'],
  css: {
    postcss: {
      plugins: [tailwindcssNesting(postcssNesting()), tailwindcss(tailwindConfig)],
    },
  },
  logLevel: 'info',
  build: {
    // dashboard chunk size is larger than the default warning limit
    chunkSizeWarningLimit: 700,
    rollupOptions: {
      output: {
        manualChunks: {
          config: ['./src/config'],
        },
      },
    },
  },
})
async function projectManagerShim(): Promise<Plugin> {
  const module = await import('./project-manager-shim-middleware')
  return {
    name: 'project-manager-shim',
    configureServer(server) {
      server.middlewares.use(module.default)
    },
    configurePreviewServer(server) {
      server.middlewares.use(module.default)
    },
  }
}
