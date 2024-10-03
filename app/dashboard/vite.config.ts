/** @file Configuration for vite. */
import * as fsSync from 'node:fs'
import * as url from 'node:url'

import vitePluginYaml from '@modyfi/vite-plugin-yaml'
import vitePluginReact from '@vitejs/plugin-react'
import * as vite from 'vite'

import * as common from 'enso-common'
import * as appConfig from 'enso-common/src/appConfig'

// =====================
// === Configuration ===
// =====================

const HTTP_STATUS_OK = 200
const SERVER_PORT = 8080
await appConfig.readEnvironmentFromFile()

/* eslint-disable @typescript-eslint/naming-convention */
export default vite.defineConfig({
  server: { port: SERVER_PORT, headers: Object.fromEntries(common.COOP_COEP_CORP_HEADERS) },
  plugins: [
    vitePluginReact({
      include: '**/*.tsx',
      babel: { plugins: ['@babel/plugin-syntax-import-attributes'] },
    }),
    vitePluginYaml(),
    serveFavicon(),
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
})

/** A plugin to serve a favicon, in development mode only. */
function serveFavicon(): vite.Plugin {
  const favicon = fsSync.readFileSync(url.fileURLToPath(new URL('./favicon.ico', import.meta.url)))
  const headers: HeadersInit = [
    ['Content-Length', String(favicon.length)],
    ['Content-Type', 'image/png'],
    ...common.COOP_COEP_CORP_HEADERS,
  ]
  return {
    name: 'serve-favicon',
    configureServer: (server) => {
      server.middlewares.use((req, res, next) => {
        if (req.url === '/favicon.ico') {
          res.writeHead(HTTP_STATUS_OK, headers).end(favicon)
        } else {
          next()
        }
      })
    },
  }
}
