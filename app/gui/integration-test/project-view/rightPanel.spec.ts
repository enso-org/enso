import type { Page } from '@playwright/test'
import { test } from 'playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import { mockCollapsedFunctionInfo, mockMethodCallInfo } from './expressionUpdates'
import { CONTROL_KEY, DELETE_KEY } from './keyboard'
import * as locate from './locate'

async function goToGraphAndGetDocs(page: Page) {
  await actions.goToGraph(page)
  await page.getByRole('tab', { name: 'Documentation' }).click()
  const docsContent = page.getByTestId('documentation-editor-content')
  await expect(docsContent.locator('.cm-line')).toExist()
  return { docsContent }
}

test('Main method documentation', async ({ page }) => {
  const { docsContent } = await goToGraphAndGetDocs(page)

  // Right-dock displays main method documentation.
  await expect(docsContent).toContainText('The main method')

  // All three images are loaded properly
  await expect(docsContent.getByAltText('Image')).toHaveCount(3)
  for (const img of await docsContent.getByAltText('Image').all())
    await expect(img).toHaveJSProperty('naturalWidth', 3)

  // Nested lists are rendered with hierarchical indentation
  const listItemPos = (text: string) =>
    docsContent
      .locator('span.cm-BulletList-item span')
      .getByText(text, { exact: true })
      .boundingBox()
  const listLevel0 = await listItemPos('Outer list element')
  const listLevel1 = await listItemPos('Nested list element')
  const listLevel2 = await listItemPos('Very nested list element')
  expect(listLevel0).not.toBeNull()
  expect(listLevel1).not.toBeNull()
  expect(listLevel2).not.toBeNull()
  expect(listLevel0!.x).toBeLessThan(listLevel1!.x)
  expect(listLevel1!.x).toBeLessThan(listLevel2!.x)

  // Documentation hotkey closes right-dock.p
  await page.keyboard.press(`${CONTROL_KEY}+D`)
  await expect(docsContent).toBeHidden()
})

test('Doc panel focus (regression #10471)', async ({ page }) => {
  const { docsContent } = await goToGraphAndGetDocs(page)

  // Open and focus code editor.
  await page.keyboard.press(`${CONTROL_KEY}+\``)
  const codeEditor = page.locator('.CodeEditor')
  await expect(codeEditor).toBeVisible()
  await codeEditor.click()

  await page.evaluate(() => {
    const codeEditorApi = (window as any).__codeEditorApi
    const docStart = codeEditorApi.indexOf('The main method')
    codeEditorApi.placeCursor(docStart + 8)
  })
  await page.keyboard.press('Space')
  await page.keyboard.press('T')
  await page.keyboard.press('E')
  await page.keyboard.press('S')
  await page.keyboard.press('T')

  const content = await page.evaluate(() => {
    const codeEditor = (window as any).__codeEditorApi
    return codeEditor.textContent()
  })
  expect(content.includes('The main TEST method')).toBe(true)
  await expect(docsContent).toContainText('The main TEST method')
})

test('Code editor with wide content does not take space from doc editor (#12476)', async ({
  page,
}) => {
  await goToGraphAndGetDocs(page)
  const rightDock = locate.rightDock(page)

  const getDocX = async () => {
    await rightDock.elementHandle().then((el) => el!.waitForElementState('stable'))
    return (await rightDock.boundingBox())!.x
  }
  const docPosWithoutCodeEditor = await getDocX()
  await page.keyboard.press(`${CONTROL_KEY}+\``)
  const codeEditor = page.locator('.CodeEditor')
  await expect(codeEditor).toBeVisible()
  const docPosWithCodeEditor = await getDocX()

  // Note that we compare `x` instead of `width`: This will catch either a change in width, or the
  // viewport becoming larger than the page (causing a change in *apparent* width).
  expect(docPosWithCodeEditor).toBe(docPosWithoutCodeEditor)
})

test('Component help', async ({ page }) => {
  await actions.goToGraph(page)

  await page.getByRole('tab', { name: 'Help' }).click()
  await expect(locate.rightDock(page)).toHaveText(/Select a single component/)

  await locate.graphNodeByBinding(page, 'final').click()
  await expect(locate.rightDock(page)).toHaveText(/No documentation available/)

  await mockMethodCallInfo(page, 'data', {
    methodPointer: {
      module: 'Standard.Base.Data',
      definedOnType: 'Standard.Base.Data',
      name: 'read',
    },
    notAppliedArguments: [0, 1, 2],
  })
  await locate.graphNodeByBinding(page, 'data').click()
  await expect(locate.rightDock(page)).toHaveText(/Reads a file into Enso/)
})

test('Documentation reflects entered function', async ({ page }) => {
  const { docsContent } = await goToGraphAndGetDocs(page)

  // Enter the collapsed function
  await mockCollapsedFunctionInfo(page, 'final', 'func1')
  await locate.graphNodeByBinding(page, 'final').dblclick()
  await expect(locate.navBreadcrumb(page)).toHaveText(['Mock Project', 'func1'])

  // Editor should contain collapsed function's docs
  await expect(docsContent).toHaveText('A collapsed function')
})

test('Link in documentation is rendered and interactive', async ({ page, context }) => {
  const { docsContent } = await goToGraphAndGetDocs(page)
  const rightDock = locate.rightDock(page)
  await expect(docsContent.locator('a')).toHaveAccessibleDescription(
    /Click to edit.*Click to open link/,
  )
  await expect(docsContent.locator('a')).toHaveText('https://example.com')
  await docsContent.locator('a').click()
  await expect(rightDock.locator('.LinkEditPopup')).toExist()
  await locate.graphEditor(page).click()
  await expect(rightDock.locator('.LinkEditPopup')).not.toBeVisible()
  const newPagePromise = new Promise<true>((resolve) => context.once('page', () => resolve(true)))
  await docsContent.locator('a').click({ modifiers: ['ControlOrMeta'] })
  await expect(() => newPagePromise).toPass({ timeout: 5000 })
})

test('Insert link button inserts link and focuses editor', async ({ page }) => {
  const { docsContent } = await goToGraphAndGetDocs(page)
  const rightDock = locate.rightDock(page)

  // Delete all text and defocus the editor
  await docsContent.locator('.cm-line').first().click()
  await page.keyboard.press(`${CONTROL_KEY}+A`)
  await page.keyboard.press(DELETE_KEY)
  await expect(docsContent.locator('.cm-line')).toBeEmpty()
  await docsContent.blur()

  // Push the button
  await rightDock.getByRole('button', { name: 'Insert link' }).click()

  // The link exists and is being edited
  await expect(docsContent.locator('a')).toExist()
  await expect(rightDock.locator('.LinkEditPopup')).toExist()
})

test('Documentation editor: Editing with keyboard', async ({ page }) => {
  const { docsContent } = await goToGraphAndGetDocs(page)

  await page.keyboard.press(`${CONTROL_KEY}+\``)
  const getGraphCode = () => page.evaluate(() => (window as any).__codeEditorApi.textContent())

  await docsContent
    .locator('.cm-line')
    .getByText(/The main method/)
    .click()
  await expect(docsContent).toBeFocused()

  await page.keyboard.press(`${CONTROL_KEY}+A`)
  const NEW_DOCS = 'New main method documentation'
  await page.keyboard.type(NEW_DOCS)
  const codeAfterSettingNewDocs = await getGraphCode()
  expect(codeAfterSettingNewDocs).toContain(`## ${NEW_DOCS}`)

  await page.keyboard.press(`${CONTROL_KEY}+Alt+1`)
  const codeAfterHeaderCommand = await getGraphCode()
  expect(codeAfterHeaderCommand).toContain(`## # ${NEW_DOCS}`)

  await page.keyboard.press('Enter')
  await page.keyboard.type('Second line')
  const codeAfterAddingLine = await getGraphCode()
  expect(codeAfterAddingLine).toContain(`## # ${NEW_DOCS}\n   Second line`)
})
