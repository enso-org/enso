/** @file Playwright browser testing configuration. */
import path from 'node:path'
import { defineConfig } from 'playwright/test'

const outputDir = process.env.TEST_UNDECLARED_OUTPUTS_DIR ?? 'test-results'

export default defineConfig({
  testDir: './tests',
  // Headless tests are run via vitest, not playwright, so we ignore them here.
  testIgnore: ['headless/**'],
  forbidOnly: !!process.env.CI,
  workers: 1,
  timeout: 180000,
  reportSlowTests: { max: 5, threshold: 60000 },
  outputDir,
  reporter: [
    ['list'],
    ['html', { outputFolder: path.join(outputDir, 'html-report'), open: 'never' }],
    ['junit', { outputFile: path.join(outputDir, 'junit.xml') }],
    ['json', { outputFile: path.join(outputDir, 'report.json') }],
  ],
  expect: {
    timeout: 30000,
    toHaveScreenshot: { threshold: 0 },
  },
  use: {
    actionTimeout: 5000,
    viewport: { width: 1380, height: 900 },
    screenshot: process.env.ENSO_PW_SCREENSHOTS ? 'only-on-failure' : 'off',
    video: process.env.ENSO_PW_VIDEO ? 'retain-on-failure' : 'off',
  },
})
