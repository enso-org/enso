/** @file Playwright browser testing configuration. */
/** Note that running Playwright in CI poses a number of issues:
 * - `backdrop-filter: blur` is disabled, due to issues with Chromium's `--disable-gpu` flag
 * (see below).
 * - System validation dialogs are not reliable between computers, as they may have different
 * default fonts. */
import * as test from '@playwright/test'

/* eslint-disable @typescript-eslint/no-magic-numbers, @typescript-eslint/strict-boolean-expressions */

export default test.defineConfig({
  testDir: './e2e',
  fullyParallel: true,
  forbidOnly: true,
  workers: 1,
  expect: {
    toHaveScreenshot: { threshold: 0 },
  },
  use: {
    baseURL: 'http://localhost:8080',
    launchOptions: {
      ignoreDefaultArgs: ['--headless'],
      args: [
        // Much closer to headful Chromium than classic headless.
        '--headless=new',
        // Required for `backdrop-filter: blur` to work.
        '--use-angle=swiftshader',
        // FIXME: `--disable-gpu` disables `backdrop-filter: blur`, which is not handled by
        // the software (CPU) compositor. This SHOULD be fixed eventually, but this flag
        // MUST stay as CI does not have a GPU.
        '--disable-gpu',
        // Fully disable GPU process.
        '--disable-software-rasterizer',
        // Disable text subpixel antialiasing.
        '--font-render-hinting=none',
        '--disable-skia-runtime-opts',
        '--disable-system-font-check',
        '--disable-font-subpixel-positioning',
        '--disable-lcd-text',
      ],
    },
  },
  webServer: {
    command: 'npm run dev:e2e',
    port: 8080,
    reuseExistingServer: false,
  },
})
