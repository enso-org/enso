/** @file Playwright browser testing configuration. */
import { defineConfig } from 'playwright/test'

export default defineConfig({
  testDir: './tests',
  // Headless tests are run via vitest, not playwright, so we ignore them here.
  testIgnore: ['headless/**'],
  forbidOnly: !!process.env.CI,
  workers: 1,
  timeout: 180000,
  reportSlowTests: { max: 5, threshold: 60000 },
  reporter: [
    ['list'],
    ['html', { outputFolder: 'html-report', open: 'never' }],
    ['json', { outputFile: 'report.json' }],
  ],
  expect: {
    timeout: 30000,
    toHaveScreenshot: { threshold: 0 },
  },
  use: {
    actionTimeout: 5000,
    viewport: { width: 1780, height: 1000 },
    screenshot: process.env.ENSO_PW_SCREENSHOTS ? 'only-on-failure' : 'off',
    video: process.env.ENSO_PW_VIDEO ? 'retain-on-failure' : 'off',
  },
})
