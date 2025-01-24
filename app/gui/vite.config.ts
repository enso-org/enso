import { sentryVitePlugin } from '@sentry/vite-plugin'
import react from '@vitejs/plugin-react'
import vue from '@vitejs/plugin-vue'
import { fileURLToPath } from 'node:url'
import postcssNesting from 'postcss-nesting'
import tailwindcss from 'tailwindcss'
import tailwindcssNesting from 'tailwindcss/nesting'
import { defaultClientConditions, defineConfig, type Plugin } from 'vite'
import VueDevTools from 'vite-plugin-vue-devtools'
import wasm from 'vite-plugin-wasm'
import tailwindConfig from './tailwind.config'
// @ts-expect-error We don't need to typecheck this file
import reactCompiler from 'babel-plugin-react-compiler'
// @ts-expect-error We don't need to typecheck this file
import syntaxImportAttributes from '@babel/plugin-syntax-import-attributes'

const isDevMode = process.env.NODE_ENV === 'development'
const isE2E = process.env.INTEGRATION_TEST === 'true'

const entrypoint = isE2E ? './src/project-view/test-entrypoint.ts' : './src/entrypoint.ts'

if (isDevMode) {
  process.env.ENSO_IDE_YDOC_SERVER_URL ||= 'ws://__HOSTNAME__:5976'
}

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
      babel: {
        plugins: [
          syntaxImportAttributes,
          [reactCompiler, { target: '18', enablePreserveExistingMemoizationGuarantees: true }],
        ],
      },
    }),
    await projectManagerShim(),
    ...((
      process.env.SENTRY_AUTH_TOKEN != null &&
      process.env.ENSO_IDE_SENTRY_ORGANIZATION != null &&
      process.env.ENSO_IDE_SENTRY_PROJECT != null
    ) ?
      [
        sentryVitePlugin({
          org: process.env.ENSO_IDE_SENTRY_ORGANIZATION,
          project: process.env.ENSO_IDE_SENTRY_PROJECT,
          ...(process.env.ENSO_IDE_VERSION != null ?
            { release: { name: process.env.ENSO_IDE_VERSION } }
          : {}),
        }),
      ]
    : []),
  ],
  optimizeDeps: {
    entries: fileURLToPath(new URL('./index.html', import.meta.url)),
    exclude: ['enso-common'],
    holdUntilCrawlEnd: true,
  },
  server: {
    headers: {
      'Cross-Origin-Opener-Policy': 'same-origin',
      'Cross-Origin-Resource-Policy': 'same-origin',
    },
    ...(process.env.GUI_HOSTNAME ? { host: process.env.GUI_HOSTNAME } : {}),
  },
  resolve: {
    conditions: isDevMode ? ['source', ...defaultClientConditions] : [...defaultClientConditions],
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
    sourcemap: true,
    rollupOptions: {
      output: {
        manualChunks: {
          config: ['./src/config'],
          entrypoint: ['./src/entrypoint'],
        },
      },
    },
  },
  preview: {
    port: 5173,
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
