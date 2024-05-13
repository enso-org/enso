/** @file Configuration for vitest. */
import * as url from 'node:url'

import * as vitestConfig from 'vitest/config'

import * as appConfig from 'enso-common/src/appConfig'

/* eslint-disable @typescript-eslint/naming-convention */

appConfig.loadTestEnvironmentVariables()
// @ts-expect-error This is required, otherwise importing node modules is broken.
// This is required for `dataLinkSchema.test.ts`.
process.env.NODE_ENV = 'development'

const VITE_CONFIG = (await import('./vite.config')).default

export default vitestConfig.mergeConfig(
  VITE_CONFIG,
  vitestConfig.defineConfig({
    test: {
      environment: 'jsdom',
      exclude: ['**/*.spec.{ts,tsx}'],
      root: url.fileURLToPath(new URL('./', import.meta.url)),
      restoreMocks: true,
    },
  })
)
