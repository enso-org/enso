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
const TIMEOUT_MS = DEBUG ? 100_000_000 : 60_000

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

const portsFromEnv = {
  projectView: parseInt(process.env.PLAYWRIGHT_PORT_PV ?? '', 10),
  dashboard: parseInt(process.env.PLAYWRIGHT_PORT ?? '', 10),
}
const ports = {
  projectView:
    Number.isFinite(portsFromEnv.projectView) ?
      portsFromEnv.projectView
    : await findFreePortInRange(4300, 4999),
  dashboard:
    Number.isFinite(portsFromEnv.dashboard) ?
      portsFromEnv.dashboard
    : await findFreePortInRange(4300, 4999),
}
console.log(`Selected playwright servers' ports: ${ports.projectView} and ${ports.dashboard}`)
// Make sure to set the env to actual port that is being used. This is necessary for workers to
// pick up the same configuration.
process.env.PLAYWRIGHT_PORT = `${ports.dashboard}`
process.env.PLAYWRIGHT_PORT_PV = `${ports.projectView}`

export default defineConfig({
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  repeatEach: process.env.CI ? 3 : 1,
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
        baseURL: `http://localhost:${ports.dashboard}`,
        actionTimeout: TIMEOUT_MS,
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
        baseURL: `http://localhost:${ports.projectView}`,
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
          `corepack pnpm build && corepack pnpm exec vite preview --port ${ports.projectView} --strictPort`
        : `corepack pnpm exec vite dev --port ${ports.projectView}`,
      // Build from scratch apparently can take a while on CI machines.
      timeout: 240 * 1000,
      port: ports.projectView,
      // We use our special, mocked version of server, thus do not want to re-use user's one.
      reuseExistingServer: false,
    },
    {
      command:
        process.env.CI || process.env.PROD ?
          `corepack pnpm exec vite -c vite.test.config.ts build && vite -c vite.test.config.ts preview --port ${ports.dashboard} --strictPort`
        : `corepack pnpm exec vite -c vite.test.config.ts --port ${ports.dashboard}`,
      timeout: 240 * 1000,
      port: ports.dashboard,
      reuseExistingServer: false,
    },
  ],
})
