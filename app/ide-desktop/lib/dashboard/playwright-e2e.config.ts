/** @file Playwright browser testing configuration. */
/** Note that running Playwright in CI poses a number of issues:
 * - `backdrop-filter: blur` is disabled, due to issues with Chromium's `--disable-gpu` flag
 * (see below).
 * - System validation dialogs are not reliable between computers, as they may have different
 * default fonts. */
import * as test from '@playwright/test'

/* eslint-disable @typescript-eslint/no-magic-numbers, @typescript-eslint/strict-boolean-expressions */

export default test.defineConfig({
    testDir: './test-e2e',
    forbidOnly: !!process.env.CI,
    retries: process.env.CI ? 2 : 0,
    ...(process.env.CI ? { workers: 1 } : {}),
    expect: {
        toHaveScreenshot: { threshold: 0 },
    },
    use: {
        baseURL: 'http://localhost:8080',
        // `--use-angle` is required for background-blur to work.
        launchOptions: {
            ignoreDefaultArgs: ['--headless'],
            args: [
                '--headless=new',
                '--use-angle=swiftshader',
                // FIXME: `--disable-gpu` disables `backdrop-filter: blur`, which is not handled by
                // the software (CPU) compositor. This SHOULD be fixed eventually, but this flag
                // MUST stay as CI does not have a GPU.
                '--disable-gpu',
                '--disable-software-rasterizer',
                '--font-render-hinting=none',
            ],
        },
    },
    webServer: {
        command: 'npx tsx test-server.ts',
        port: 8080,
        reuseExistingServer: !process.env.CI,
    },
})
