/** @file Playwright browser testing configuration. */
import { defineConfig } from 'playwright/test'

export default defineConfig({
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
    // Disabled as it logs all environment variables, which may contain sensitive data.
    trace: 'off',
    actionTimeout: 5000,
  },
})
