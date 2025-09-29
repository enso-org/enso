/** @file Commonly used functions for electron tests */
/* eslint-disable no-empty-pattern */

import { TEXTS } from 'enso-common/src/text'
import fs from 'node:fs/promises'
import os from 'node:os'
import path from 'node:path'
import {
  _electron,
  test as base,
  expect,
  type ElectronApplication,
  type Locator,
  type Page,
} from 'playwright/test'

const LOADING_TIMEOUT = 10000
const TEXT = TEXTS.english
export const CONTROL_KEY = os.platform() === 'darwin' ? 'Meta' : 'Control'
const TEST_USER_FILE = path.join(import.meta.dirname, '../playwright/.auth/user.json')

const credentials = JSON.parse(
  await fs.readFile(TEST_USER_FILE, { encoding: 'utf-8' }).catch((err) => {
    throw Error('Cannot read Test User credentials.', { cause: err })
  }),
)

const electronExecutablePath = await (async () => {
  const POSSIBLE_EXEC_PATHS = [
    '../../../dist/ide/linux-unpacked/enso',
    '../../../dist/ide/win-unpacked/Enso.exe',
    '../../../dist/ide/mac/Enso.app/Contents/MacOS/Enso',
    '../../../dist/ide/mac-arm64/Enso.app/Contents/MacOS/Enso',
  ].map((p) => path.resolve(import.meta.dirname, p))
  try {
    const promises = POSSIBLE_EXEC_PATHS.map((p) => fs.access(p, fs.constants.X_OK).then(() => p))
    return await Promise.any(promises)
  } catch {
    throw Error('Cannot find Enso package')
  }
})()

/**
 * Tests run on electron executable.
 *
 * Similar to playwright's test, but launches electron, and passes Page of the main window.
 */
export const test = base.extend<{
  testRunId: string
  projectsDir: string
  app: ElectronApplication
  page: Page
}>({
  testRunId: async function ({}, use, testInfo) {
    await use(`${testInfo.testId}-${Date.now()}`)
  },
  projectsDir: async function ({ testRunId }, use) {
    const projectsDir = path.join(os.tmpdir(), 'enso-test-projects', testRunId)
    await use(projectsDir)
  },

  /**
   * Setup for all tests: Create an electron-based app instance.
   */
  app: async function ({ projectsDir, testRunId }, use) {
    const args = process.env.ENSO_TEST_APP_ARGS?.split(',') ?? []
    const app = await _electron.launch({
      executablePath: electronExecutablePath,
      args,
      env: { ...process.env, ENSO_TEST: 'true', ENSO_TEST_PROJECTS_DIR: projectsDir },
    })
    // Set the password as global var before turning on tracing.
    // This way it will be not disclosed to anyone downloading traces of failed tests.
    ;(await app.firstWindow()).evaluate((password) => {
      ;(window as any).passwordOverride = password
    }, credentials.password)
    await app.context().tracing.start({ screenshots: true, snapshots: true, sources: true })
    await use(app)
    await app.context().tracing.stop({ path: `test-traces/${testRunId}.zip` })
    await app.close()
  },
  page: async function ({ app, viewport }, use) {
    const innerPage = await app.firstWindow()
    if (viewport) innerPage.setViewportSize(viewport)
    await use(innerPage)
  },
})

/**
 * Login as test user. This function asserts that page is the login page, and uses
 * credentials from playwright/.auth/user.json file.
 */
export async function loginAsTestUser(page: Page) {
  // Login screen
  await expect(page.getByText('Login to your account')).toBeVisible({ timeout: LOADING_TIMEOUT })
  await expect(page.getByRole('textbox', { name: 'email' })).toBeVisible()
  await expect(page.getByRole('textbox', { name: 'password' })).toBeVisible()
  await page.getByRole('textbox', { name: 'email' }).fill(credentials.user)
  // Put some placeholder - the actual password was set in fixture (see above).
  await page.getByRole('textbox', { name: 'password' }).fill('mellon')
  await page.getByRole('button', { name: TEXT.login, exact: true }).click()

  await page
    .getByRole('group', { name: TEXT.licenseAgreementCheckbox })
    .getByText(TEXT.licenseAgreementCheckbox)
    .click()
  await page
    .getByRole('group', { name: TEXT.privacyPolicyCheckbox })
    .getByText(TEXT.privacyPolicyCheckbox)
    .click()

  await page.getByRole('button', { name: TEXT.accept }).click()
}

/**
 * The funcion creates a new Enso project
 */
export async function createNewProject(page: Page) {
  const newProjectTab = page.getByRole('button', { name: 'New Project', exact: true })

  await expect(newProjectTab).toBeVisible()
  await newProjectTab.click()
  await expect(page.locator('.GraphNode')).toHaveCount(1, { timeout: 60000 })

  const tableViz = page.locator('.TableVisualization')
  await expect(tableViz).toBeVisible({ timeout: 30000 })
  await expect(tableViz).toContainText('Welcome To Enso!')
}

/**
 * If welcome project is to be opened, this function takes you back to your dashboard
 */
export async function closeWelcome(page: Page) {
  const welcomeProjectTab = page.getByRole('tab', { name: 'Getting Started with Enso' })
  await Promise.race([welcomeProjectTab.waitFor({ state: 'visible' }), page.waitForTimeout(3000)])
  if (await welcomeProjectTab.isVisible()) {
    await page.getByRole('tab', { name: 'Data Catalog' }).click()
  }
}

/**
 * Finds the "newest" project (highest numbered "New Project N") in the user dasboard.
 * @param page - The Playwright Page instance
 * @returns Locator for the newest project
 */
export async function getNewestProject(page: Page): Promise<Locator> {
  // Returning back to the data catalog
  const dataCatalogTab = page.getByRole('tab', { name: 'Data Catalog' })
  await expect(dataCatalogTab).toBeVisible()
  await dataCatalogTab.click()

  const projects = await page
    .getByTestId('drive-view')
    .getByText(/New Project \d+/)
    .all()

  const numbered = await Promise.all(
    projects.map(async (p) => {
      const text = await p.innerText()
      const num = parseInt(text.replace('New Project ', ''), 10)
      return { locator: p, num }
    }),
  )

  return numbered.reduce((a, b) => (a.num > b.num ? a : b)).locator
}
