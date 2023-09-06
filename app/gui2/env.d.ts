/// <reference types="vite/client" />

module 'y-websocket' {
  // hack for bad module resolution
  export * from 'node_modules/y-websocket/dist/src/y-websocket'
}
