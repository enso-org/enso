/** @file Playwright browser testing configuration. */
import * as test from '@playwright/test'

export default test.defineConfig({
    testDir: './test-e2e',
})
