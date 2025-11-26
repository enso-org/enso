import { expect, test, type Page } from 'integration-test/base'
import { mockExpressionUpdate } from './expressionUpdates'
import * as locate from './locate'

const ACCEPT_INPUT_SHORTCUT = `ControlOrMeta+Enter`

async function deselectAllNodes(page: Page) {
  await page.keyboard.press('Escape')
  await expect(locate.selectedNodes(page)).toHaveCount(0)
}

async function expectAndCancelBrowser(
  page: Page,
  expectedText: string,
  expectedLabel: string | null,
  expectedSelfArgument?: string,
) {
  await test.step(`expectAndCancelBrowser ${expectedText} ${expectedLabel} ${expectedSelfArgument}`, async () => {
    const nodeCount = await locate.graphNode(page).count()
    await expect(locate.componentBrowser(page)).toExist()
    await expect(locate.componentBrowserEntry(page)).toExist()
    await expect(page.locator('[data-transitioning]')).toHaveCount(0)
    if (expectedLabel != null) {
      await expect(page.getByTestId('component-editor-label')).toContainText(expectedLabel)
    } else {
      await expect(page.getByTestId('component-editor-label')).toBeHidden()
    }
    if (expectedSelfArgument != null) {
      await expect(locate.componentBrowser(page)).toHaveAttribute(
        'data-self-argument',
        expectedSelfArgument,
      )
    }
    await expect(locate.componentBrowserInput(page)).toHaveText(expectedText)
    await expect(locate.componentBrowserInput(page)).toBeInViewport()
    await page.keyboard.press('Escape')
    await expect(locate.componentBrowser(page)).toBeHidden()
    await expect(page.locator('[data-transitioning]')).toHaveCount(0)
    await expect(locate.graphNode(page)).toHaveCount(nodeCount)
  })
}

test('Different ways of opening Component Browser', async ({ editorPage, page }) => {
  await editorPage

  // Without source node

  // (+) button
  await locate.addNewNodeButton(page).click()
  await expectAndCancelBrowser(page, '', 'Input')
  // (+) button with selection (ignored)
  await locate.graphNodeByBinding(page, 'selected').click()
  await locate.addNewNodeButton(page).click()
  await expectAndCancelBrowser(page, '', 'Input')
  // Enter key
  await locate.graphEditor(page).click({ position: { x: 100, y: 500 } })
  await locate.graphEditor(page).press('Enter')
  await expectAndCancelBrowser(page, '', 'Input')

  // With source node

  // Enter key
  await mockExpressionUpdate(page, 'selected', {
    type: ['Standard.Table.Table.Table'],
  })
  await locate.graphNodeByBinding(page, 'selected').click()
  await locate.graphEditor(page).press('Enter')
  await expectAndCancelBrowser(page, '', 'Table', 'selected')
  // Click-drag the edge (click, move mouse, click again)
  let outputPort = await locate.outputPortCoordinates(
    page,
    locate.graphNodeByBinding(page, 'selected'),
  )
  await page.mouse.click(outputPort.x, outputPort.y)
  await locate.graphEditor(page).click({ position: { x: 100, y: 500 } })
  await expectAndCancelBrowser(page, '', 'Table', 'selected')

  // Double-clicking port
  // TODO[ao] Without timeout, even the first click would be treated as double due to previous
  // event. Probably we need a better way to simulate double clicks.
  await page.waitForTimeout(600)
  outputPort = await locate.outputPortCoordinates(page, locate.graphNodeByBinding(page, 'selected'))
  await page.mouse.click(outputPort.x, outputPort.y)
  await page.mouse.click(outputPort.x, outputPort.y)
  await expectAndCancelBrowser(page, '', 'Table', 'selected')

  // Dragging out an edge (click and hold, move mouse, release)
  await page.mouse.move(outputPort.x, outputPort.y)
  await page.waitForTimeout(600) // Avoid double clicks, see TODO above.
  await page.mouse.down({ button: 'left' })
  await page.mouse.move(outputPort.x + 300, outputPort.y + 400)
  await page.mouse.up({ button: 'left' })
  await expectAndCancelBrowser(page, '', 'Table', 'selected')
})

test('Opening Component Browser from output port buttons', async ({ editorPage, page }) => {
  await editorPage

  // Pan the graph up so that every node is guaranteed to be visible.
  await page.mouse.move(100, 100)
  await page.mouse.down({ button: 'middle' })
  await page.mouse.move(100, -350)
  await page.mouse.up({ button: 'middle' })

  // Small (+) button shown when node is hovered
  const node = locate.graphNodeByBinding(page, 'table')
  await locate.graphNodeIcon(node).hover()
  const createNodeFromPortButton = await locate.createNodeFromPortButton(page, node)
  await expect(createNodeFromPortButton).toBeVisible()
  await createNodeFromPortButton.click({ force: true })
  await expectAndCancelBrowser(page, '', null, 'table')

  // Small (+) button shown when node is selected
  await page.keyboard.press('Escape')
  await node.click()
  await expect(createNodeFromPortButton).toBeVisible()
  await createNodeFromPortButton.click({ force: true })
  await expectAndCancelBrowser(page, '', null, 'table')

  // Small (+) button can be dragged
  const bbox = await createNodeFromPortButton.boundingBox()
  if (!bbox) throw new Error('Bounding box not found')
  await page.mouse.move(bbox.x, bbox.y)
  await page.mouse.down({ button: 'left' })
  await page.mouse.move(bbox.x + 300, bbox.y + 400)
  await page.mouse.up({ button: 'left' })
  await expectAndCancelBrowser(page, '', null, 'table')
})

test('Graph Editor pans to Component Browser', async ({ editorPage, page }) => {
  await editorPage

  // Select node, pan out of view of it, press Enter; should pan to show node and CB
  await locate.graphNodeByBinding(page, 'final').click()
  await page.mouse.move(100, 180)
  await page.mouse.down({ button: 'middle' })
  await page.mouse.move(100, 1300)
  await page.mouse.up({ button: 'middle' })
  await expect(locate.graphNodeByBinding(page, 'final')).not.toBeInViewport()
  await locate.graphEditor(page).press('Enter')
  await expect(locate.graphNodeByBinding(page, 'final')).toBeInViewport()
  await expectAndCancelBrowser(page, '', null)

  // Dragging out an edge to the bottom of the viewport; when the CB pans into view, some nodes are out of view.
  await page.mouse.move(100, 1100)
  await page.mouse.down({ button: 'middle' })
  await page.mouse.move(100, 280)
  await page.mouse.up({ button: 'middle' })
  await expect(locate.graphNodeByBinding(page, 'five')).toBeInViewport()
  const outputPort = await locate.outputPortCoordinates(
    page,
    locate.graphNodeByBinding(page, 'final'),
  )
  await page.mouse.click(outputPort.x, outputPort.y)
  await locate.graphEditor(page).click({ position: { x: 100, y: 1700 } })
  await expect(locate.graphNodeByBinding(page, 'five')).not.toBeInViewport()
  await expectAndCancelBrowser(page, '', null)
})

test('Accepting suggestion', async ({ editorPage, page }) => {
  // Clicking entry
  await editorPage
  await locate.addNewNodeButton(page).click()
  let nodeCount = await locate.graphNode(page).count()
  await locate.componentBrowserEntry(page).nth(1).click()
  await expect(locate.componentBrowser(page)).toBeHidden()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount + 1)
  await expect(locate.graphNode(page).last().locator('.WidgetToken')).toHaveText([
    'Data',
    '.',
    'read_many',
  ])
  await expect(locate.graphNode(page).last()).toBeSelected()

  // Clicking at highlighted entry
  nodeCount = await locate.graphNode(page).count()
  await deselectAllNodes(page)
  await locate.addNewNodeButton(page).click()
  await locate.componentBrowserSelectedEntry(page).first().click()
  await expect(locate.componentBrowser(page)).toBeHidden()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount + 1)
  await expect(locate.graphNode(page).last().locator('.WidgetToken')).toHaveText([
    'Data',
    '.',
    'read',
  ])
  await expect(locate.graphNode(page).last()).toBeSelected()

  // Accepting with Enter
  nodeCount = await locate.graphNode(page).count()
  await deselectAllNodes(page)
  await locate.addNewNodeButton(page).click()
  await expect(locate.componentBrowserInput(page)).toBeFocused()
  await page.keyboard.press('Enter')
  await expect(locate.componentBrowser(page)).toBeHidden()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount + 1)
  await expect(locate.graphNode(page).last().locator('.WidgetToken')).toHaveText([
    'Data',
    '.',
    'read',
  ])
  await expect(locate.graphNode(page).last()).toBeSelected()
})

test('Accepting any written input', async ({ editorPage, page }) => {
  await editorPage
  await locate.addNewNodeButton(page).click()
  const nodeCount = await locate.graphNode(page).count()
  await locate.componentBrowserInput(page).fill('re')
  await page.keyboard.press(ACCEPT_INPUT_SHORTCUT)
  await expect(locate.componentBrowser(page)).toBeHidden()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount + 1)
  await expect(locate.graphNode(page).last().locator('.WidgetToken')).toHaveText('re')
})

test('Filling input with suggestion', async ({ editorPage, page }) => {
  await editorPage
  await locate.addNewNodeButton(page).click()
  await expect(locate.componentBrowser(page)).toExist()
  await expect(locate.componentBrowserEntry(page)).toExist()

  // Applying suggestion
  await page.keyboard.press('Shift+Enter')
  await expect(locate.componentBrowser(page)).toExist()
  await expect(locate.componentBrowserInput(page)).toHaveText('Data.read ')
})

test('Filtering list', async ({ editorPage, page }) => {
  await editorPage
  await locate.addNewNodeButton(page).click()
  await locate.componentBrowserInput(page).fill('re_ma')
  const segments = locate.componentBrowserEntry(page).locator('.component-label-segment')
  await expect(segments).toHaveText(['Data.', 're', 'ad', '_ma', 'ny'])
  const highlighted = locate.componentBrowserEntry(page).locator('.component-label-segment.match')
  await expect(highlighted).toHaveText(['re', '_ma'])
  // Filtered-out group are hidden, and the rest displays number of matched elements.
  await expect(page.locator('.groupEntry')).toHaveText(['all (1)', 'File (1)'])
})

test('Navigating components', async ({ editorPage, page }) => {
  await editorPage
  await locate.addNewNodeButton(page).click()
  await expect(locate.componentBrowserSelectedEntry(page)).toExist()
  await expect(locate.componentBrowserSelectedEntry(page)).toHaveText('Data.read')
  await expect(page.locator('.documentationContent > p')).toHaveText([
    'Reads a file into Enso.',
    'Returns: Any',
  ])
  await page.keyboard.press('ArrowDown')
  await expect(locate.componentBrowserSelectedEntry(page)).toHaveText('Data.read_many')
  await expect(page.locator('.documentationContent > p')).toHaveText([
    'Reads a list of files into Enso.',
    'Returns: Any',
  ])
  await page.getByRole('button', { name: 'Show Help' }).click()
  await expect(locate.rightDock(page)).toBeVisible()
})

test('Navigating groups', async ({ editorPage, page }) => {
  await editorPage
  await locate.addNewNodeButton(page).click()
  await expect(locate.componentBrowserSelectedEntry(page)).toExist()
  await expect(page.locator('.groupEntry')).toHaveText([
    'suggestions',
    'File',
    'Web',
    'DateTime',
    'Constants',
    'Conversions',
  ])
  await expect(locate.componentBrowserEntry(page)).toHaveText([
    'Data.read',
    'Data.read_many',
    'Data.fetch',
    'Table.input',
    'Data.post',
    'Date_Time.now',
  ])

  // Hover first group
  await page.locator('.groupEntry').nth(1).hover()
  // Wait for view update: "File" group has only two entries
  await expect(locate.componentBrowserEntry(page)).toHaveCount(2)
  await expect(locate.componentBrowserEntryByLabel(page, 'Data.read')).toExist()
  await expect(locate.componentBrowserEntryByLabel(page, 'Date_Time.now')).toHaveCount(0)
  await expect(locate.componentBrowserSelectedEntry(page)).toExist() // component list didn't lose focus.

  // Navigate to second group using arrows.
  await page.keyboard.press('Tab')
  await expect(locate.componentBrowserSelectedEntry(page)).toHaveCount(0)
  await page.keyboard.press('ArrowDown')
  await expect(locate.componentBrowserSelectedEntry(page)).toHaveCount(0)
  await expect(page.locator('.groupEntry.selected')).toHaveText('Web')
  await expect(locate.componentBrowserEntryByLabel(page, 'Data.read')).toHaveCount(0)
  await expect(locate.componentBrowserEntryByLabel(page, 'Data.fetch')).toExist()
  await page.keyboard.press('Tab')
  await expect(locate.componentBrowserSelectedEntry(page)).toExist()
})

test('Editing existing nodes', async ({ editorPage, page }) => {
  await editorPage
  const node = locate.graphNodeByBinding(page, 'data')
  const ADDED_PATH = '"/home/enso/Input.txt"'

  // Start node editing
  await locate.graphNodeIcon(node).click({ modifiers: ['ControlOrMeta'] })
  await expect(locate.componentBrowser(page)).toBeVisible()
  await expect(page.getByTestId('component-editor-label')).toBeHidden()
  const content = locate.componentBrowserInput(page)
  await expect(content).toHaveText('Data.read')

  // Add argument and accept - assume the editor is already focused.
  await page.keyboard.press('End')
  await page.keyboard.type(` ${ADDED_PATH}`)
  await expect(content).toHaveText(`Data.read ${ADDED_PATH}`)
  await page.keyboard.press('Enter')
  await expect(locate.componentBrowser(page)).toBeHidden()
  await expect(node.locator('.WidgetToken')).toHaveText([
    'Data',
    '.',
    'read',
    /["\u{201C}]/u,
    /["\u{201D}]/u,
  ])
  await expect(node.getByTestId('widget-text-content')).toHaveText(ADDED_PATH.replaceAll('"', ''))

  // Edit again, using "edit" button
  await locate.graphNodeIcon(node).click()
  await node.getByTestId('more-button').click()
  await node.getByTestId('action:component.startEditing').click()
  await expect(locate.componentBrowser(page)).toBeVisible()
  await expect(content).toHaveText(`Data.read ${ADDED_PATH}`)
  for (let i = 0; i < ADDED_PATH.length; ++i) await page.keyboard.press('Backspace')
  await expect(content).toHaveText('Data.read ')
  await page.keyboard.press('Enter')
  await expect(locate.componentBrowser(page)).toBeHidden()
  await expect(node.locator('.WidgetToken')).toHaveText(['Data', '.', 'read'])
  await expect(node.locator('.WidgetText')).toBeHidden()
})

test('Visualization preview: type-based visualization selection', async ({ editorPage, page }) => {
  await editorPage
  const nodeCount = await locate.graphNode(page).count()
  await locate.addNewNodeButton(page).click()
  await expect(locate.componentBrowser(page)).toExist()
  await expect(locate.componentBrowserEntry(page)).toExist()
  const content = locate.componentBrowserInput(page)
  await content.fill('Table.ne')
  await expect(content).toHaveText('Table.ne')
  await page.keyboard.press(`Shift+Enter`)
  await expect(locate.tableVisualization(page)).toBeVisible()
  await page.keyboard.press('Escape')
  await expect(locate.componentBrowser(page)).toBeHidden()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount)
})

test('Visualization preview: user visualization selection', async ({ editorPage, page }) => {
  await editorPage
  const nodeCount = await locate.graphNode(page).count()
  await locate.addNewNodeButton(page).click()
  await expect(locate.componentBrowser(page)).toExist()
  const content = locate.componentBrowserInput(page)
  await content.fill('4')
  await expect(content).toHaveText('4')
  await page.keyboard.press(`Shift+Enter`)
  await expect(locate.jsonVisualization(page)).toBeVisible()
  await expect(locate.jsonVisualization(page)).toContainText('"visualizedExpr": "4"')
  await locate.toggleVisualizationSelectorButton(page).click()
  await page
    .getByTestId('visualization-selector-entries')
    .getByRole('button', { name: 'Table' })
    .click()
  await expect(locate.tableVisualization(page)).toBeVisible()
  await page.keyboard.press('Escape')
  await expect(locate.componentBrowser(page)).toBeHidden()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount)
})

// TODO[#10949]: the record button on node is disabled.
test.skip('Component browser handling of overridden record-mode', async ({ editorPage, page }) => {
  await editorPage
  const node = locate.graphNodeByBinding(page, 'data')
  const ADDED_PATH = '"/home/enso/Input.txt"'
  const recordModeToggle = node.getByTestId('toggleRecord')
  const recordModeIndicator = node.getByTestId('recordingOverriddenButton')

  // Enable record mode for the node.
  await locate.graphNodeIcon(node).hover()
  await expect(recordModeToggle).toHaveClass(/toggledOff/)
  await recordModeToggle.click()
  await expect(recordModeToggle).toHaveClass(/toggledOn/)
  await page.keyboard.press('Escape')
  // TODO[ao]: The simple move near top-left corner not always works i.e. not always
  //  `pointerleave` event is emitted. Investigated in https://github.com/enso-org/enso/issues/9478
  //  once fixed, remember to change the second `await page.mouse.move(700, 1200, { steps: 20 })`
  //  line below.
  await page.mouse.move(700, 1200, { steps: 20 })
  await expect(recordModeIndicator).toBeVisible()
  await locate.graphNodeIcon(node).hover()
  await expect(recordModeToggle).toHaveClass(/toggledOn/)
  // Ensure editing in the component browser doesn't display the override expression.
  await locate.graphNodeIcon(node).click({ modifiers: ['ControlOrMeta'] })
  await expect(locate.componentBrowser(page)).toBeVisible()
  const content = locate.componentBrowserInput(page)
  await expect(content).toHaveText('Data.read')
  // Ensure committing an edit doesn't change the override state.
  await page.keyboard.press('End')
  await content.pressSequentially(` ${ADDED_PATH}`)
  await page.keyboard.press('Enter')
  await expect(locate.componentBrowser(page)).toBeHidden()
  // See TODO above.
  await page.mouse.move(700, 1200, { steps: 20 })
  await expect(recordModeIndicator).toBeVisible()
  // Ensure after editing the node, editing still doesn't display the override expression.
  await locate.graphNodeIcon(node).click({ modifiers: ['ControlOrMeta'] })
  await expect(locate.componentBrowser(page)).toBeVisible()
  await expect(content).toHaveText(`Data.read ${ADDED_PATH}`)
})

test('AI prompt', async ({ editorPage, page }) => {
  await editorPage

  const node = locate.graphNodeByBinding(page, 'data')
  await node.click()
  await expect(node).toBeSelected()
  await locate.graphEditor(page).press('Enter')
  await expect(locate.componentBrowser(page)).toBeVisible()

  await page.keyboard.insertText('AI:convert to table')
  await expect(page.locator('.ComponentList')).toBeHidden()
  await page.keyboard.press('Enter')
  await expect(locate.componentBrowserInput(page)).toHaveText('to_table')
  await expect(locate.componentBrowser(page)).toHaveAttribute('data-self-argument', 'data')
})
