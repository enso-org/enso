import { defineConfig, mergeConfig } from 'vite'

import config from './vite.config'

export default mergeConfig(
  config,
  defineConfig({
    build: {
      minify: false,
      // Cannot be enabled as it causes the JavaScript heap to be out of memory
      // sourcemap: true,
    },
  }),
)
