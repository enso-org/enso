import wasm from 'vite-plugin-wasm'
import { defineConfig, mergeConfig } from 'vitest/config'
import nodeVitestConfig from '../vitest.node.base'

export default mergeConfig(
  nodeVitestConfig,
  defineConfig({
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
  }),
)
