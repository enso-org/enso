/** @file Commonly used functions for electron tests */
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
const TEST_USER_FILE = path.join(import.meta.dirname, '../playwright/.auth/user.json')
const POSSIBLE_ELECTRON_PATHS = [
  '../../../dist/ide/linux-unpacked/enso',
  '../../../dist/ide/win-unpacked/Enso.exe',
  '../../../dist/ide/mac/Enso.app/Contents/MacOS/Enso',
  '../../../dist/ide/mac-arm64/Enso.app/Contents/MacOS/Enso',
]

export const credentials: { readonly user: string; readonly password: string } = await fs
  .readFile(TEST_USER_FILE, { encoding: 'utf-8' })
  .then(
    (contents) => JSON.parse(contents),
    (error) => {
      throw new Error(`Cannot read Test User credentials from '${TEST_USER_FILE}'.`, {
        cause: error,
      })
    },
  )
  .catch((error) => {
    throw new Error(`Cannot parse Test User credentials from '${TEST_USER_FILE}'.`, {
      cause: error,
    })
  })

export const electronExecutablePath = await (async () => {
  try {
    const promises = POSSIBLE_ELECTRON_PATHS.map((p) => path.resolve(import.meta.dirname, p)).map(
      (p) => fs.access(p, fs.constants.X_OK).then(() => p),
    )
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
  // eslint-disable-next-line no-empty-pattern
  testRunId: async function ({}, use, testInfo) {
    await use(`${testInfo.titlePath.join('-')}-${Date.now()}`)
  },
  projectsDir: async function ({ testRunId }, use) {
    const projectsDir = path.join(os.tmpdir(), 'enso-test-projects', testRunId)
    await use(projectsDir)
  },

  /** Setup for all tests: Create an electron-based app instance. */
  app: async function ({ projectsDir, testRunId }, use) {
    const args = process.env.ENSO_TEST_APP_ARGS?.split(',') ?? []
    const app = await _electron.launch({
      executablePath: electronExecutablePath,
      args,
      env: {
        ...process.env,
        ENSO_TEST: 'true',
        ENSO_TEST_PROJECTS_DIR: projectsDir.replace(/\\/g, '/'),
      },
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
 * Login as test user - assert that page is the login page, and use credentials from
 * `playwright/.auth/user.json`.
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

  await expect(
    page
      .getByRole('group', { name: TEXT.licenseAgreementCheckbox })
      .getByText(TEXT.licenseAgreementCheckbox),
  ).toBeVisible({ timeout: 60000 })
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

/** Create a new Enso project */
export async function createNewProject(page: Page) {
  await page.getByRole('button', { name: 'New Project' }).click()
  await expect(page.locator('.GraphNode')).toHaveCount(1, { timeout: 60000 })

  const tableViz = page.locator('.TableVisualization')
  await expect(tableViz).toContainText('Welcome To Enso!')
}

/** If welcome project is to be opened, navigate back to the dashboard. */
export async function closeWelcome(page: Page) {
  const welcomeProjectTab = page.getByRole('tab', { name: 'Getting Started with Enso Analytics' })
  const loadingIndicator = welcomeProjectTab.locator('.LoadingSpinner')
  await Promise.race([
    welcomeProjectTab
      .waitFor({ state: 'visible', timeout: 0 })
      .then(() => loadingIndicator.waitFor({ state: 'hidden' })),
    page.waitForTimeout(3000),
  ])
  if (await welcomeProjectTab.isVisible()) {
    await welcomeProjectTab.locator('.CloseButton').click()
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
  await dataCatalogTab.click()

  await expect(page.getByTestId('drive-view')).toBeVisible({ timeout: LOADING_TIMEOUT })
  const projectsLocator = page.getByTestId('drive-view').getByText(/New Project \d+/)
  await expect(projectsLocator).not.toHaveCount(0)

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

/**
 * Click the eye button, visualizing component data
 */
export async function visualizeData(page: Page) {
  const showViz = page.getByLabel('Show visualization (Space)')
  await showViz.click({ timeout: 5000 })
}

/**
 * Open new component browser refefencing the last created component
 */
export async function createNewComponent(page: Page) {
  const moreButton = page.getByTestId('more-button').getByRole('button', { name: 'More' }).last()
  await moreButton.click()

  await page.keyboard.press('Enter')
}

/**
 * Open new component browser based on the name of referenced parent component
 */
export async function openComponentBrowser(page: Page, parentComponent: string) {
  await page.getByText(parentComponent, { exact: true }).click()
  await page.keyboard.press('Enter')
}

/**
 * Find textbox located in parent component and fill in text value
 */
export async function fillWidgetText(
  page: Page,
  containerName: string,
  value: string,
  index?: number,
) {
  const cont = page.getByText(containerName)

  const box = cont.getByTestId('widget-text-content')
  if (index) return box.nth(index).fill(value)
  else return box.fill(value)
}

/**
 * Wait for the Samples folder download
 * This function retries to access passed file every 5 sec, fails after 1 min
 */
export async function waitForDownload(pathToFile: string): Promise<void> {
  const start = Date.now()
  while (true) {
    try {
      await fs.access(pathToFile) // ✅ file exists
      return
    } catch {
      if (Date.now() - start > 60_000) {
        throw new Error(`File ${pathToFile} not found within 60 seconds`)
      }
      await new Promise((r) => setTimeout(r, 5_000))
    }
  }
}

/** Open drop-down menu in WidgetSelection with given label. */
export function openDropdownInWidget(page: Page, label: string) {
  return page.locator('.WidgetSelection', { hasText: new RegExp(`^${label}$`) }).click()
}

/** Find and click + button in an empty Vector Widget inside provided locator. */
export function addFirstElementToWidgetVector(locator: Locator) {
  return locator.getByRole('list').filter({ hasText: /^$/ }).getByLabel('Add a new item').click()
}
