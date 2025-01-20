import wasm from 'vite-plugin-wasm'
import { defineConfig } from 'vitest/config'

export default defineConfig({
  plugins: [wasm()],
  build: {
    lib: {
      entry: 'src/index.ts',
      formats: ['es'],
    },
  },
  resolve: {
    conditions: ['source'],
  },
})
