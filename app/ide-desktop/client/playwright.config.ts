/** @file Playwright browser testing configuration. */
import { ChromaticConfig } from '@chromatic-com/playwright'
import { defineConfig } from '@playwright/test'

export default defineConfig<ChromaticConfig>({
  testDir: './tests',
  forbidOnly: !!process.env.CI,
  workers: 1,
  timeout: 180000,
  reportSlowTests: { max: 5, threshold: 60000 },
  expect: {
    timeout: 30000,
    toHaveScreenshot: { threshold: 0 },
  },
  use: {
    actionTimeout: 5000,
    disableAutoSnapshot: true,
  },
})
