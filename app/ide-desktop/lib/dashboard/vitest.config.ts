/** @file Configuration for vitest. */
import * as url from 'node:url'

import * as vitestConfig from 'vitest/config'

import viteConfig from './vite.config'

export default vitestConfig.mergeConfig(
  viteConfig,
  vitestConfig.defineConfig({
    test: {
      environment: 'jsdom',
      exclude: ['**/*.spec.{ts,tsx}'],
      root: url.fileURLToPath(new URL('./', import.meta.url)),
      restoreMocks: true,
    },
  })
)
