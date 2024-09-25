/** @file Playwright browser testing configuration. */
import { defineConfig } from '@playwright/test'

/* eslint-disable @typescript-eslint/no-magic-numbers, @typescript-eslint/strict-boolean-expressions */

export default defineConfig({
  testDir: './tests',
  forbidOnly: !!process.env.CI,
  workers: 1,
  timeout: 60000,
  globalSetup: './tests/setup.ts',
  expect: {
    timeout: 5000,
    toHaveScreenshot: { threshold: 0 },
  },
  use: {
    actionTimeout: 5000,
  },
})
