import { fileURLToPath, URL } from 'node:url'

import { defineConfig, Plugin } from 'vite'
import vue from '@vitejs/plugin-vue'
import postcssNesting from 'postcss-nesting'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [vue(), yWebsocketServer()],
  resolve: {
    alias: {
      '@': fileURLToPath(new URL('./src', import.meta.url)),
    },
  },

  css: {
    postcss: {
      plugins: [postcssNesting],
    },
  },
})

function yWebsocketServer(): Plugin {
  return {
    name: 'y-websocket-server',
    configureServer(server) {
      server.ws.on('connection', (socket) => {
        console.log('socket connected')
      })
    },
  }
}
