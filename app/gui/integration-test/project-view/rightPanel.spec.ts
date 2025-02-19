import { test } from 'playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import { mockCollapsedFunctionInfo, mockMethodCallInfo } from './expressionUpdates'
import { CONTROL_KEY } from './keyboard'
import * as locate from './locate'

test('Main method documentation', async ({ page }) => {
  await actions.goToGraph(page)

  const rightDock = locate.rightDock(page)
  // Documentation panel hotkey opens right-dock.
  await expect(rightDock).toBeHidden()
  await page.keyboard.press(`${CONTROL_KEY}+D`)
  await expect(rightDock).toBeVisible()

  // Right-dock displays main method documentation.
  await expect(locate.editorRoot(rightDock)).toContainText('The main method')
  // All three images are loaded properly
  await expect(rightDock.getByAltText('Image')).toHaveCount(3)
  for (const img of await rightDock.getByAltText('Image').all())
    await expect(img).toHaveJSProperty('naturalWidth', 3)

  // Nested lists are rendered with hierarchical indentation
  const listItemPos = (text: string) =>
    locate
      .editorRoot(rightDock)
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
  await expect(locate.rightDock(page)).toBeHidden()
})

test('Doc panel focus (regression #10471)', async ({ page }) => {
  await actions.goToGraph(page)

  await page.keyboard.press(`${CONTROL_KEY}+D`)
  await page.keyboard.press(`${CONTROL_KEY}+\``)
  await expect(locate.rightDock(page)).toBeVisible()
  const codeEditor = page.locator('.CodeEditor')
  await expect(codeEditor).toBeVisible()

  // Focus code editor.
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
  await expect(locate.rightDock(page)).toContainText('The main TEST method')
})

test('Component help', async ({ page }) => {
  await actions.goToGraph(page, false)
  await locate.rightDock(page).getByRole('button', { name: 'Help' }).click()
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
  await actions.goToGraph(page)

  // Open the panel
  await expect(locate.rightDock(page)).toBeHidden()
  await page.keyboard.press(`${CONTROL_KEY}+D`)
  await expect(locate.rightDock(page)).toBeVisible()

  // Enter the collapsed function
  await mockCollapsedFunctionInfo(page, 'final', 'func1')
  await locate.graphNodeByBinding(page, 'final').dblclick()
  await expect(locate.navBreadcrumb(page)).toHaveText(['Mock Project', 'func1'])

  // Editor should contain collapsed function's docs
  await expect(locate.editorRoot(locate.rightDock(page))).toHaveText('A collapsed function')
})

test('Link in documentation is rendered and interactive', async ({ page, context }) => {
  await actions.goToGraph(page)
  await page.keyboard.press(`${CONTROL_KEY}+D`)
  await expect(locate.rightDock(page)).toBeVisible()
  const docs = locate.editorRoot(locate.rightDock(page)).first()
  await expect(docs.locator('a')).toHaveAccessibleDescription(/Click to edit.*Click to open link/)
  await expect(docs.locator('a')).toHaveText('https://example.com')
  await docs.locator('a').click()
  await expect(docs.locator('.LinkEditPopup')).toExist()
  await locate.graphEditor(page).click()
  await expect(docs.locator('.LinkEditPopup')).not.toBeVisible()
  const newPagePromise = new Promise<true>((resolve) => context.once('page', () => resolve(true)))
  await docs.locator('a').click({ modifiers: ['ControlOrMeta'] })
  await expect(() => newPagePromise).toPass({ timeout: 5000 })
})
