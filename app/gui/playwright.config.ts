/** @file Playwright browser testing configuration. */
/**
 * Note that running Playwright in CI poses a number of issues:
 * - `backdrop-filter: blur` is disabled, due to issues with Chromium's `--disable-gpu` flag
 * (see below).
 * - System validation dialogs are not reliable between computers, as they may have different
 * default fonts.
 */
import { defineConfig } from '@playwright/test'
import net from 'net'

const DEBUG = process.env.DEBUG_E2E === 'true'
const TIMEOUT_MS = DEBUG ? 100_000_000 : 30_000

async function findFreePortInRange(min: number, max: number) {
  for (let i = 0; i < 50; i++) {
    const portToCheck = Math.floor(Math.random() * (max - min + 1)) + min
    if (await checkAvailablePort(portToCheck)) return portToCheck
  }
  throw new Error('Failed to find a free port.')
}

function checkAvailablePort(port: number) {
  return new Promise((resolve, reject) => {
    const server = net.createServer()
    server
      .unref()
      .on('error', (e: any) => ('EADDRINUSE' === e.code ? resolve(false) : reject(e)))
      .listen({ host: '0.0.0.0', port }, () => server.close(() => resolve(true)))
  })
}

const portFromEnv = parseInt(process.env.PLAYWRIGHT_PORT ?? '', 10)
const PORT = Number.isFinite(portFromEnv) ? portFromEnv : await findFreePortInRange(4300, 4999)
console.log(`Selected playwright server port: ${PORT}`)
// Make sure to set the env to actual port that is being used. This is necessary for workers to
// pick up the same configuration.
process.env.PLAYWRIGHT_PORT = `${PORT}`

export default defineConfig({
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  // TODO[ao]: from dashboard - why this way?
  // workers: process.env.PROD ? 8 : 1,
  repeatEach: process.env.CI ? 3 : 1,
  ...(process.env.CI ? { workers: 1 } : {}),
  reporter: 'html',
  use: {
    headless: !DEBUG,
    actionTimeout: 5000,
    trace: 'retain-on-failure',
    ...(DEBUG ?
      {}
    : {
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
      }),
  },
  projects: [
    {
      name: 'Dashboard',
      testDir: './e2e/dashboard',
      expect: {
        toHaveScreenshot: { threshold: 0 },
        timeout: TIMEOUT_MS,
      },
      timeout: TIMEOUT_MS,
      use: {
        baseURL: `http://localhost:${PORT + 1}`,
      },
    },
    {
      name: 'Setup Tests for Project View',
      testMatch: /e2e\/project-view\/setup\.ts/,
    },
    {
      name: 'Project View',
      dependencies: ['Setup Tests for Project View'],
      testDir: './e2e/project-view',
      timeout: 60000,
      expect: {
        timeout: 5000,
        toHaveScreenshot: { threshold: 0 },
      },
      use: {
        viewport: { width: 1920, height: 1750 },
        baseURL: `http://localhost:${PORT}`,
      },
    },
  ],
  webServer: [
    {
      env: {
        E2E: 'true',
      },
      command:
        process.env.CI || process.env.PROD ?
          `corepack pnpm build && corepack pnpm exec vite preview --port ${PORT} --strictPort`
        : `corepack pnpm exec vite dev --port ${PORT}`,
      // Build from scratch apparently can take a while on CI machines.
      timeout: 240 * 1000,
      port: PORT,
      // We use our special, mocked version of server, thus do not want to re-use user's one.
      reuseExistingServer: false,
    },
    {
      command:
        process.env.CI || process.env.PROD ?
          `corepack pnpm exec vite -c vite.test.config.ts build && vite preview --port ${PORT + 1} --strictPort`
        : `corepack pnpm exec vite -c vite.test.config.ts --port ${PORT + 1}`,
      port: PORT + 1,
      reuseExistingServer: false,
    },
  ],
})
