import { fileURLToPath } from 'node:url'

import { defineConfig, Plugin } from 'vite'
import vue from '@vitejs/plugin-vue'
import postcssNesting from 'postcss-nesting'
import { WebSocketServer } from 'ws'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [vue(), yWebsocketServer()],
  resolve: {
    alias: {
      shared: fileURLToPath(new URL('./shared', import.meta.url)),
      '@': fileURLToPath(new URL('./src', import.meta.url)),
    },
  },
  css: {
    postcss: {
      plugins: [postcssNesting],
    },
  },
})

const roomNameRegex = /^[a-z0-9-]+$/i

function yWebsocketServer(): Plugin {
  return {
    name: 'y-websocket-server',
    configureServer(server) {
      if (server.httpServer == null) return
      const { setupWSConnection } = require('./node_modules/y-websocket/bin/utils')
      const wss = new WebSocketServer({ noServer: true })
      wss.on('connection', setupWSConnection)
      server.httpServer.on('upgrade', (request, socket, head) => {
        if (request.url != null && request.url.startsWith('/room/')) {
          const docName = request.url.slice(6)
          if (roomNameRegex.test(docName)) {
            wss.handleUpgrade(request, socket, head, (ws) => {
              wss.emit('connection', ws, request, { docName })
            })
          }
        }
      })
    },
  }
}
