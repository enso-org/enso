/** @file Configuration for vitest. */
import * as url from 'node:url'

import * as vitestConfig from 'vitest/config'

// @ts-expect-error This is required, otherwise importing node modules is broken.
// This is required for `datalinkSchema.test.ts`.
process.env.NODE_ENV = 'development'

const VITE_CONFIG = (await import('./vite.config')).default

export default vitestConfig.mergeConfig(
  VITE_CONFIG,
  vitestConfig.defineConfig({
    test: {
      environment: 'jsdom',
      exclude: [...vitestConfig.configDefaults.exclude, '**/*.spec.{ts,tsx}'],
      root: url.fileURLToPath(new URL('./', import.meta.url)),
      restoreMocks: true,
    },
  }),
)
