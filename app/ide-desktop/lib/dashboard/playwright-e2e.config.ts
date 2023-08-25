/** @file Playwright browser testing configuration. */
import * as test from '@playwright/test'

/* eslint-disable @typescript-eslint/no-magic-numbers, @typescript-eslint/strict-boolean-expressions */

export default test.defineConfig({
    testDir: './test-e2e',
    forbidOnly: !!process.env.CI,
    retries: process.env.CI ? 2 : 0,
    ...(process.env.CI ? { workers: 1 } : {}),
    use: {
        baseURL: 'http://localhost:8080',
        // This MUST be `false`, otherwise `background-blur` does not work.
        // (`--headless=new` does not fix this issue.)
        headless: false,
    },
    webServer: {
        command: 'npx tsx test-server.ts',
        port: 8080,
        reuseExistingServer: !process.env.CI,
    },
})
