import type EditorPageActions from 'integration-test/actions/EditorPageActions'
import { expect, test, type Locator, type Page } from 'integration-test/base'
import {
  clearMockWidgetConfigurations,
  restoreMockWidgetConfigurations,
} from 'integration-test/mock/lsHandler'
import * as actions from './actions'
import { mockMethodCallInfo } from './expressionUpdates'
import * as locate from './locate'

class DropDownLocator {
  readonly rootWidget: Locator
  readonly dropDown: Locator
  readonly dropDownAnyState: Locator
  readonly items: Locator
  readonly selectedItems: Locator

  constructor(ancestor: Locator, widget: string = 'WidgetSelection') {
    this.rootWidget = ancestor.locator(`.${widget}`).first()
    const page = ancestor.page()
    // There can be only one open dropdown at a time on a page. We have to filter out the ones that
    // still have leaving animation running.
    this.dropDown = page.locator('.DropdownWidget:not([data-transitioning])')
    this.dropDownAnyState = page.locator('.DropdownWidget')
    this.items = this.dropDown.locator('.item')
    this.selectedItems = this.dropDown.locator('.item.selected')
  }

  async expectVisibleWithOptions(options: string[]): Promise<void> {
    await this.expectVisible()
    const page = this.dropDown.page()
    await expect(this.items.first()).toBeVisible()
    for (const option of options) {
      await expect(
        this.items.filter({ has: page.getByText(option, { exact: true }) }),
      ).toBeVisible()
    }
    await expect(this.items).toHaveCount(options.length)
  }

  async expectVisible(): Promise<void> {
    await expect(this.dropDown).toHaveCount(1)
    await expect(this.dropDown).toBeVisible()
  }

  async expectNotVisible(): Promise<void> {
    await expect(this.dropDownAnyState).toBeHidden()
  }

  async clickOption(option: string): Promise<void> {
    const item = this.item(option)
    await item.click()
  }

  async clickWidget(): Promise<void> {
    await this.rootWidget.click()
  }

  selectedItem(text: string): Locator {
    const page = this.dropDown.page()
    return this.selectedItems.filter({ has: page.getByText(text) })
  }

  item(text: string): Locator {
    const page = this.dropDown.page()
    return this.items.filter({ has: page.getByText(text) })
  }
}

const CHOOSE_CLOUD_FILE = 'Choose file in cloud…'
const CHOOSE_LOCAL_FILE = 'Choose file…'
const CHOOSE_FILE_OPTIONS = [CHOOSE_CLOUD_FILE, CHOOSE_LOCAL_FILE]

test('Widget in plain AST', async ({ editorPage, page }) => {
  await editorPage
  const numberNode = locate.graphNodeByBinding(page, 'five')
  const numberWidget = numberNode.locator('.WidgetNumber')
  await expect(numberWidget).toBeVisible()
  await expect(numberWidget).toHaveValue('5')

  const listNode = locate.graphNodeByBinding(page, 'list')
  const listWidget = listNode.locator('.WidgetVector')
  await expect(listWidget).toBeVisible()

  const textNode = locate.graphNodeByBinding(page, 'text')
  const textWidget = textNode.locator('.WidgetText')
  await expect(textWidget).toBeVisible()
  await expect(textWidget.getByTestId('widget-text-content')).toHaveText('test')
})

test('Text widget: Convert to multiline', async ({ editorPage, page }) => {
  await editorPage
  const textNode = locate.graphNodeByBinding(page, 'text')
  const textWidget = textNode.locator('.WidgetText')
  await expect(textWidget).toBeVisible()
  await expect(textWidget.getByTestId('widget-text-content')).toHaveText('test')
  await textWidget.click()
  await expect(textWidget.getByTestId('widget-text-content')).toBeFocused()
  await page.keyboard.press('ArrowRight')
  await page.keyboard.press('Alt+Enter')
  await page.keyboard.insertText('Next line')
  await page.keyboard.press('Enter')
  await expect(textWidget.getByTestId('widget-text-content')).not.toBeFocused()
  await expect(textWidget.getByTestId('widget-text-content').locator('.cm-line')).toHaveText([
    'test',
    'Next line',
  ])
})

test.describe('Multi-selection widget', () => {
  test.beforeEach(async ({ editorPage, page }) => {
    await editorPage

    await mockMethodCallInfo(page, 'selected', {
      methodPointer: {
        module: 'Standard.Table.Table',
        definedOnType: 'Standard.Table.Table.Table',
        name: 'select_columns',
      },
      notAppliedArguments: [1],
    })
  })

  async function openAndCheckDropdown(page: Page) {
    // Click the argument to open the dropdown.
    const node = locate.graphNodeByBinding(page, 'selected')
    const topLevelArgs = node.locator('.WidgetTopLevelArgument')
    await expect(topLevelArgs).toHaveCount(1)
    const columnsArg = topLevelArgs.filter({ has: page.getByText('columns') })

    // Get the dropdown and corresponding vector; they both have 0 items.
    const dropDown = new DropDownLocator(columnsArg, 'WidgetMultiSelection')
    await dropDown.clickWidget()
    await dropDown.expectVisibleWithOptions(['Column A', 'Column B'])
    const vector = node.locator('.WidgetVector')
    const vectorItems = vector.getByTestId('list-item-content').getByTestId('widget-text-content')
    await expect(vector).toBeVisible()
    await expect(dropDown.selectedItems).toHaveCount(0)
    await expect(vectorItems).toHaveCount(0)

    return { dropDown, vector, vectorItems }
  }

  test('Enabling and disabling items', async ({ page }) => {
    const { dropDown, vector, vectorItems } = await openAndCheckDropdown(page)

    // Enable an item.
    await dropDown.clickOption('Column A')
    await expect(dropDown.selectedItem('Column A')).toExist()
    await expect(vector).toBeVisible()
    await expect(vectorItems).toHaveCount(1)
    await expect(vectorItems.first()).toHaveText('Column A')
    await dropDown.expectVisible()

    // Enable another item.
    await dropDown.clickOption('Column B')
    await expect(vectorItems).toHaveCount(2)
    await expect(vectorItems.first()).toHaveText('Column A')
    await expect(vectorItems.nth(1)).toHaveText('Column B')
    await expect(dropDown.dropDown).toBeVisible()
    await expect(dropDown.items).toHaveCount(2)
    await expect(dropDown.selectedItems).toHaveCount(2)

    // Disable an item.
    await dropDown.clickOption('Column A')
    await expect(vectorItems).toHaveCount(1)
    await expect(vectorItems.first()).toHaveText('Column B')
    await expect(dropDown.dropDown).toBeVisible()
    await expect(dropDown.items).toHaveCount(2)
    await expect(dropDown.selectedItems).toHaveCount(1)

    // Disable the last item.
    await dropDown.clickOption('Column B')
    await expect(vectorItems).toHaveCount(0)
    await expect(dropDown.dropDown).toBeVisible()
    await expect(dropDown.items).toHaveCount(2)
    await expect(dropDown.selectedItems).toHaveCount(0)
  })

  test('Interactions: Clicking in dropdown after editing item', async ({ page }) => {
    const { dropDown, vectorItems } = await openAndCheckDropdown(page)

    // Enable an item.
    await dropDown.clickOption('Column A')
    await expect(vectorItems.first()).toHaveText('Column A')
    await dropDown.expectVisible()

    // Click to edit an item.
    await vectorItems.first().click()
    await expect(vectorItems.first()).toBeFocused()
    await expect(dropDown.dropDown).toBeVisible()

    // Enable another item.
    // In a bug caught while testing a PR, this step fails to add an item to the vector because the
    // `pointerdown` in the dropdown closes it before the click is handled.
    await dropDown.clickOption('Column B')
    await expect(vectorItems).toHaveCount(2)
    await expect(dropDown.dropDown).toBeVisible()
  })

  test('Add-item button', async ({ page }) => {
    const { dropDown, vector } = await openAndCheckDropdown(page)

    await dropDown.clickOption('Column A')
    await dropDown.expectVisible()
    await page.keyboard.press('Escape')
    await dropDown.expectNotVisible()

    await locate.addItemButton(vector).click()
    await expect(dropDown.items).toHaveCount(2)
    await expect(dropDown.selectedItems).toHaveCount(1)
  })

  test('Editing items', async ({ page }) => {
    const { dropDown, vectorItems } = await openAndCheckDropdown(page)

    await dropDown.clickOption('Column A')
    await dropDown.clickOption('Column B')
    await page.keyboard.press('Escape')
    await dropDown.expectNotVisible()

    // Clicking to edit an item opens the dropdown.
    await expect(vectorItems.first()).toHaveText('Column A')
    await vectorItems.first().click()
    await expect(vectorItems.first()).toBeFocused()
    await expect(dropDown.dropDown).toBeVisible()

    // Clicking to edit a different item doesn't close the dropdown.
    await expect(vectorItems.nth(1)).toHaveText('Column B')
    await vectorItems.nth(1).click()
    await expect(vectorItems.nth(1)).toBeFocused()
    await expect(dropDown.dropDown).toBeVisible()

    // Edit an item.
    await expect(dropDown.selectedItem('Column A')).toExist()
    await expect(dropDown.selectedItem('Column B')).toExist()
    await expect(vectorItems.first()).toHaveText('Column A')
    await vectorItems.first().fill('Something Else')
    await expect(dropDown.selectedItem('Column A')).toBeHidden()
    await expect(dropDown.selectedItem('Column B')).toExist()
  })
})

test('Editing list', async ({ editorPage, page }) => {
  await editorPage
  const node = locate.graphNodeByBinding(page, 'autoscoped')
  const vector = node.locator('.WidgetVector')
  const vectorItems = vector.locator('.item')
  const vectorElements = vector.getByTestId('list-item-content')
  await expect(vectorElements).toHaveText(['..Group_By'])
  await node.click()

  // Test add
  await locate.addItemButton(node).click()
  await locate.addItemButton(node).click()
  await expect(vectorElements).toHaveText(['..Group_By', '_', '_'])

  // Test drag: remove item
  const secondItem = vectorItems.nth(1).locator('[draggable]')
  await secondItem.dragTo(secondItem, {
    force: true,
    targetPosition: { x: 200, y: 0 },
  })
  await expect(vectorElements).toHaveText(['..Group_By', '_'])

  // Test drag: reorder items
  await vectorItems.nth(1).locator('[draggable]').hover()
  await page.mouse.down()
  // `dragenter` / `dragleave` events are not dispatched reliably without multiple mouse movements
  await vectorElements.first().hover({ position: { x: 10, y: 10 }, force: true })
  await vectorElements.first().hover({ position: { x: 15, y: 10 }, force: true })
  await vectorElements.first().hover({ position: { x: 20, y: 10 }, force: true })
  await vectorElements.first().hover({ position: { x: 30, y: 10 }, force: true })
  await locate.graphEditor(page).hover({ position: { x: 100, y: 300 } })
  await expect(vectorElements).toHaveText(['..Group_By'])
  await expect(vector.getByTestId('dragPlaceholder')).toHaveCount(0)
  await vectorElements.first().hover({ position: { x: 10, y: 10 }, force: true })
  await vectorElements.first().hover({ position: { x: 15, y: 10 }, force: true })
  await vectorElements.first().hover({ position: { x: 20, y: 10 }, force: true })
  await vectorElements.first().hover({ position: { x: 30, y: 10 }, force: true })
  await expect(vector.getByTestId('dragPlaceholder')).toHaveCount(1)
  await page.mouse.up()
  await expect(vectorElements).toHaveText(['_', '..Group_By'])

  // Test delete
  await locate.deleteItemButton(vectorItems.first()).click()
  await expect(vectorElements).toHaveText(['..Group_By'])

  // Test delete: last item
  await locate.deleteItemButton(vectorItems).click()
  await expect(vectorItems).toBeHidden()
  await expect(vector).toExist()
})

async function dataReadNodeWithMethodCallInfo(editorPage: EditorPageActions): Promise<Locator> {
  return await editorPage
    .mockMethodCallInfo('data', {
      methodPointer: {
        module: 'Standard.Base.Data',
        definedOnType: 'Standard.Base.Data',
        name: 'read',
      },
      notAppliedArguments: [0, 1, 2],
    })
    .locateNodes('data')
}

test.describe('Dynamic configuration updates', () => {
  test.beforeEach(clearMockWidgetConfigurations)
  test.afterEach(restoreMockWidgetConfigurations)

  /**
   * Check that dynamic dropdowns (with items provided by widget configuration) are not shown
   * until dynamic configuration arrives from the engine.
   * This test uses `format` argument of the `Data.read`, which has both static tags and a
   * dynamic configuration.
   * We check that no dropdown is shown until dynamic configuration arrives.
   */
  test('Dynamic dropdown', async ({ page, localApi, editorPage }) => {
    const node = await dataReadNodeWithMethodCallInfo(editorPage)
    const topLevelArgs = node.locator('.WidgetTopLevelArgument')
    await node.click()
    await expect(topLevelArgs).toHaveCount(3)

    // No dropdown is shown until dynamic configuration arrives.
    await expect(
      topLevelArgs.filter({ has: page.getByText('format') }).locator('.WidgetSelection'),
    ).not.toBeVisible()

    // Provide dynamic configuration for `format` arg.
    await localApi.updateVisualization('.read', [
      [
        'format',
        {
          type: 'Widget',
          constructor: 'Single_Choice',
          label: null,
          values: [
            {
              type: 'Choice',
              constructor: 'Option',
              value: '..Csv',
              label: 'Csv',
              parameters: [],
            },
            {
              type: 'Choice',
              constructor: 'Option',
              value: '..Excel',
              label: 'Excel',
              parameters: [],
            },
          ],
          display: { type: 'Display', constructor: 'Always' },
        },
      ],
    ])

    const formatArg = topLevelArgs.filter({ has: page.getByText('format') })
    const formatDropdown = new DropDownLocator(formatArg)
    await formatArg.click()
    await formatDropdown.expectVisibleWithOptions(['Csv', 'Excel'])
  })

  /**
   * Check that numeric widget is displayed even if dynamic configuration is not provided.
   * Unlike dynamic dropdowns, numeric widget can work normally without dynamic configuration.
   */
  test('Number widget', async ({ page, localApi, editorPage }) => {
    await editorPage
    const node = locate.graphNodeByBinding(page, 'selected')
    await locate.graphNodeIcon(node).click({ modifiers: ['ControlOrMeta'] })
    await expect(locate.componentBrowser(page)).toBeVisible()
    const content = locate.componentBrowserInput(page)
    await page.keyboard.press('End')
    await page.keyboard.type(` 1`)
    await expect(content).toHaveText(`select_columns 1`)
    await page.keyboard.press('Enter')
    await expect(locate.componentBrowser(page)).toBeHidden()
    await mockMethodCallInfo(page, 'selected', {
      methodPointer: {
        module: 'Standard.Table.Table',
        definedOnType: 'Standard.Table.Table.Table',
        name: 'select_columns',
      },
      notAppliedArguments: [2, 3, 4, 5],
    })
    const topLevelArgs = node.locator('.WidgetTopLevelArgument')
    await expect(topLevelArgs).toHaveCount(5)
    await expect(node.locator('.WidgetNumber')).toBeVisible()
    await expect(node.locator('.WidgetNumber')).toHaveValue('1')
    // Input has no slider, because there is no limits information.
    await expect(node.locator('.AutoSizedInput')).not.toHaveClass(/slider/)

    // Provide limits from dynamic configuration.
    await localApi.updateVisualization('.select_columns', [
      [
        'columns',
        {
          type: 'Widget',
          constructor: 'Numeric_Input',
          maximum: 10,
          minimum: 0,
          display: { type: 'Display', constructor: 'Always' },
        },
      ],
    ])
    // Slider is now displayed.
    await expect(node.locator('.AutoSizedInput')).toHaveClass(/slider/)
  })

  /**
   * File browser widget is weird. We want to match it even when dynamic configuration is not yet provided,
   * but it also uses a dropdown widget internally. This is why we have a special test case for it.
   */
  test('File browser widget', async ({ page, localApi, editorPage }) => {
    const node = await dataReadNodeWithMethodCallInfo(editorPage)
    await node.click()
    await expect(node.locator('.WidgetTopLevelArgument')).toHaveCount(3)
    const pathArg = node.locator('.WidgetTopLevelArgument').filter({ has: page.getByText('path') })
    await pathArg.click()
    const pathDropdown = new DropDownLocator(pathArg)
    await pathDropdown.expectVisibleWithOptions([...CHOOSE_FILE_OPTIONS])
    // Provide dynamic configuration for `path` argument.
    // (we use Folder_Browser here, and check how dropdown items have changed)
    await localApi.updateVisualization('.read', [
      [
        'path',
        {
          type: 'Widget',
          constructor: 'Folder_Browse',
          display: { type: 'Display', constructor: 'Always' },
        },
      ],
    ])
    await pathDropdown.expectVisibleWithOptions(['Choose directory…', 'Choose directory in cloud…'])
  })

  /**
   * Some widgets inherit their configuration from a parent widget.
   * Inherited config has a priority even if newer configuration for the child expression arrives.
   * We are using `aggregated` node to test this.
   */
  test('Inherited configuration', async ({ page, localApi, editorPage }) => {
    await editorPage.mockMethodCallInfo('aggregated', {
      methodPointer: {
        module: 'Standard.Table.Table',
        definedOnType: 'Standard.Table.Table.Table',
        name: 'aggregate',
      },
      notAppliedArguments: [1, 2, 3, 4],
    })
    const node = locate.graphNodeByBinding(page, 'aggregated')
    await node.click()
    await expect(node.locator('.WidgetTopLevelArgument')).toHaveCount(4)
    // Top-level configuration, including configuration for child widgets.
    await localApi.updateVisualization('.aggregate', [
      [
        'columns',
        {
          type: 'Widget',
          constructor: 'Single_Choice',
          label: null,
          values: [
            {
              type: 'Choice',
              constructor: 'Option',
              value: 'Standard.Table.Aggregate_Column.Aggregate_Column.Group_By',
              label: 'Group By',
              parameters: [
                [
                  'column',
                  {
                    type: 'Widget',
                    constructor: 'Single_Choice',
                    label: null,
                    values: [
                      {
                        type: 'Choice',
                        constructor: 'Option',
                        value: '"column 1"',
                        label: 'column 1',
                        parameters: [],
                      },
                      {
                        type: 'Choice',
                        constructor: 'Option',
                        value: '"column 2"',
                        label: 'column 2',
                        parameters: [],
                      },
                    ],
                    display: { type: 'Display', constructor: 'Always' },
                  },
                ],
              ],
            },
          ],
          display: { type: 'Display', constructor: 'Always' },
        },
      ],
    ])
    const columnsArg = node
      .locator('.WidgetTopLevelArgument')
      .filter({ has: page.getByText('columns', { exact: true }) })
    await columnsArg.click()
    const columnsDropdown = new DropDownLocator(columnsArg)
    await columnsDropdown.expectVisibleWithOptions(['Group By'])
    await columnsDropdown.clickOption('Group By')
    await expect(columnsArg.locator('.WidgetToken')).toContainText([
      'Aggregate_Column',
      '.',
      'Group_By',
    ])
    await mockMethodCallInfo(
      page,
      {
        binding: 'aggregated',
        expr: 'Aggregate_Column.Group_By',
      },
      {
        methodPointer: {
          module: 'Standard.Table.Aggregate_Column',
          definedOnType: 'Standard.Table.Aggregate_Column.Aggregate_Column',
          name: 'Group_By',
        },
        notAppliedArguments: [0, 1],
      },
    )
    const firstItem = columnsArg.locator('.WidgetPort > .WidgetSelection').nth(0)
    const firstItemDropdown = new DropDownLocator(firstItem)
    await firstItemDropdown.clickWidget()
    await firstItemDropdown.expectVisibleWithOptions(['column 1', 'column 2'])

    // Provide dynamic configuration for `column` argument of `Aggregate_Column.Group_By`.
    // It shouldn’t affect the selectable variants of the dropdown, because parent
    // config has a priority.
    await localApi.updateVisualization('.Group_By', [
      [
        'column',
        {
          type: 'Widget',
          constructor: 'Single_Choice',
          label: null,
          values: [
            {
              type: 'Choice',
              constructor: 'Option',
              value: '"column 3"',
              label: 'column 3',
              parameters: [],
            },
            {
              type: 'Choice',
              constructor: 'Option',
              value: '"column 4"',
              label: 'column 4',
              parameters: [],
            },
          ],
          display: { type: 'Display', constructor: 'Always' },
        },
      ],
    ])
    await firstItemDropdown.expectVisibleWithOptions(['column 1', 'column 2'])

    // Update parent configuration
    await localApi.updateVisualization('.aggregate', [
      [
        'columns',
        {
          type: 'Widget',
          constructor: 'Single_Choice',
          label: null,
          values: [
            {
              type: 'Choice',
              constructor: 'Option',
              value: 'Standard.Table.Aggregate_Column.Aggregate_Column.Group_By',
              label: 'Group By',
              parameters: [
                [
                  'column',
                  {
                    type: 'Widget',
                    constructor: 'Single_Choice',
                    label: null,
                    values: [
                      {
                        type: 'Choice',
                        constructor: 'Option',
                        value: '"column 5"',
                        label: 'column 5',
                        parameters: [],
                      },
                      {
                        type: 'Choice',
                        constructor: 'Option',
                        value: '"column 6"',
                        label: 'column 6',
                        parameters: [],
                      },
                    ],
                    display: { type: 'Display', constructor: 'Always' },
                  },
                ],
              ],
            },
          ],
          display: { type: 'Display', constructor: 'Always' },
        },
      ],
    ])
    await firstItemDropdown.expectVisibleWithOptions(['column 5', 'column 6'])
  })
})

test('Selection widgets in Data.read node', async ({ editorPage, page }) => {
  const node = await dataReadNodeWithMethodCallInfo(editorPage)
  // Check initially visible arguments
  const topLevelArgs = node.locator('.WidgetTopLevelArgument')
  await expect(topLevelArgs).toHaveCount(1)

  // Check arguments after selecting node
  await node.click()
  await expect(topLevelArgs).toHaveCount(3)

  // Set value on `on_problems` (static drop-down)
  const onProblemsArg = topLevelArgs.filter({ has: page.getByText('on_problems') })
  await onProblemsArg.click()
  const onProblemsDropdown = new DropDownLocator(onProblemsArg)
  await onProblemsDropdown.expectVisibleWithOptions([
    '..Ignore',
    '..Report_Warning',
    '..Report_Error',
  ])
  await onProblemsDropdown.clickOption('Report_Error')
  await expect(onProblemsArg.locator('.WidgetToken')).toContainText(['..', 'Report_Error'])

  // Change value on `on_problems`
  await mockMethodCallInfo(page, 'data', {
    methodPointer: {
      module: 'Standard.Base.Data',
      definedOnType: 'Standard.Base.Data',
      name: 'read',
    },
    notAppliedArguments: [0, 1],
  })
  await page.getByText('Report_Error').click()
  await onProblemsDropdown.expectVisibleWithOptions([
    '..Ignore',
    '..Report_Warning',
    '..Report_Error',
  ])
  await onProblemsDropdown.clickOption('Report_Warning')
  await expect(onProblemsArg.locator('.WidgetToken')).toContainText(['..', 'Report_Warning'])

  // Set value on `path` (dynamic config)
  const pathArg = topLevelArgs.filter({ has: page.getByText('path') })
  await pathArg.click()
  const pathDropdown = new DropDownLocator(pathArg)
  await pathDropdown.expectVisibleWithOptions([...CHOOSE_FILE_OPTIONS, 'File 1', 'File 2'])
  await pathDropdown.clickOption('File 2')
  await expect(pathArg.getByTestId('widget-text-content')).toHaveText('File 2')

  // Change value on `path` (dynamic config)
  await mockMethodCallInfo(page, 'data', {
    methodPointer: {
      module: 'Standard.Base.Data',
      definedOnType: 'Standard.Base.Data',
      name: 'read',
    },
    notAppliedArguments: [1],
  })
  await page.getByText('path').click()
  await pathDropdown.expectVisibleWithOptions([...CHOOSE_FILE_OPTIONS, 'File 1', 'File 2'])
  await pathDropdown.clickOption('File 1')
  await expect(pathArg.getByTestId('widget-text-content')).toHaveText('File 1')
})

test('Selection widget with text widget as input', async ({ editorPage, page }) => {
  const node = await dataReadNodeWithMethodCallInfo(editorPage)
  const topLevelArgs = node.locator('.WidgetTopLevelArgument')
  const pathArg = topLevelArgs.filter({ has: page.getByText('path') })
  const pathDropdown = new DropDownLocator(pathArg)
  const pathArgInput = pathArg.getByTestId('widget-text-content')
  await pathArg.click()
  await pathDropdown.expectVisible()
  await pathDropdown.clickOption('File 2')
  await expect(pathArgInput).toHaveText('File 2')

  // Editing text input shows and filters drop down
  await pathArgInput.click()
  await pathDropdown.expectVisibleWithOptions([...CHOOSE_FILE_OPTIONS, 'File 1', 'File 2'])
  // Using `type` instead of `inputText` here to catch keydown bugs like #13505.
  await page.keyboard.type('File 1')
  await pathDropdown.expectVisibleWithOptions(['File 1'])
  // Clearing input should show all options
  await pathArgInput.clear()
  await pathDropdown.expectVisibleWithOptions([...CHOOSE_FILE_OPTIONS, 'File 1', 'File 2'])

  // When a filter doesn't match any entries, the dropdown is hidden.
  await page.keyboard.insertText('No such entry')
  await pathDropdown.expectNotVisible()
  // If the text is changed so that the entries list is no longer empty, the dropdown returns.
  await pathArgInput.clear()
  await pathDropdown.expectVisibleWithOptions([...CHOOSE_FILE_OPTIONS, 'File 1', 'File 2'])

  // Esc should cancel editing and close drop down
  await page.keyboard.press('Escape')
  await expect(pathArgInput).not.toBeFocused()
  await expect(pathArgInput).toHaveText('File 2')
  await expect(pathDropdown.dropDown).toBeHidden()

  // Choosing entry should finish editing
  await pathArgInput.click()
  await pathDropdown.expectVisibleWithOptions([...CHOOSE_FILE_OPTIONS, 'File 1', 'File 2'])
  await page.keyboard.insertText('File')
  await pathDropdown.expectVisibleWithOptions(['File 1', 'File 2'])
  await pathDropdown.clickOption('File 1')
  await expect(pathArgInput).not.toBeFocused()
  await expect(pathArgInput).toHaveText('File 1')
  await expect(pathDropdown.dropDown).toBeHidden()

  // Clicking-off and pressing Enter should accept text as-is
  await pathArgInput.click()
  await pathDropdown.expectVisibleWithOptions([...CHOOSE_FILE_OPTIONS, 'File 1', 'File 2'])
  await page.keyboard.insertText('File')
  await page.keyboard.press('Enter')
  await expect(pathArgInput).not.toBeFocused()
  await expect(pathArgInput).toHaveText('File')
  await expect(pathDropdown.dropDown).toBeHidden()

  await pathArgInput.click()
  await pathDropdown.expectVisibleWithOptions([...CHOOSE_FILE_OPTIONS, 'File 1', 'File 2'])
  await page.keyboard.insertText('Foo')
  await expect(pathArgInput).toHaveText('Foo')
  await actions.clickAtBackground(page)
  await expect(pathArgInput).not.toBeFocused()
  await expect(pathArgInput).toHaveText('Foo')
  await expect(pathDropdown.dropDown).toBeHidden()
})

test('File Browser widget', async ({ editorPage, page }) => {
  await editorPage
  await mockMethodCallInfo(page, 'data', {
    methodPointer: {
      module: 'Standard.Base.Data',
      definedOnType: 'Standard.Base.Data',
      name: 'read',
    },
    notAppliedArguments: [0, 1, 2],
  })
  // Wait for arguments to load.
  const node = locate.graphNodeByBinding(page, 'data')
  const topLevelArgs = node.locator('.WidgetTopLevelArgument')
  await expect(topLevelArgs).toHaveCount(1)
  const pathArg = topLevelArgs.filter({ has: page.getByText('path') })
  const pathDropdown = new DropDownLocator(pathArg)
  await pathArg.click()
  await pathDropdown.expectVisibleWithOptions([...CHOOSE_FILE_OPTIONS, 'File 1', 'File 2'])
  await pathDropdown.clickOption(CHOOSE_LOCAL_FILE)
  await expect(pathArg.getByTestId('widget-text-content')).toHaveText('/path/to/some/mock/file')
})

test.describe('Table expression', () => {
  function prepare(editorPage: EditorPageActions) {
    return editorPage
      .mockMethodCallInfo('table', {
        methodPointer: {
          module: 'Standard.Table.Table',
          definedOnType: 'Standard.Table.Table.Table',
          name: 'set',
        },
        notAppliedArguments: [],
      })
      .mockMethodCallInfo(
        { binding: 'table', expr: 'expr ""' },
        {
          methodPointer: {
            module: 'Standard.Table.Expression',
            definedOnType: 'Standard.Table.Expression',
            name: 'expr',
          },
          notAppliedArguments: [],
        },
      )
  }

  test('Language recognized', async ({ page, editorPage }) => {
    await prepare(editorPage)
    const tableNode = locate.graphNodeByBinding(page, 'table')
    const exprText = tableNode.locator('.WidgetText')
    await expect(exprText).toHaveAttribute('data-text-syntax', 'enso-table-expression')
  })

  test('Autocomplete: Builtins', async ({ editorPage }) => {
    await prepare(editorPage)
    const ac = await getTableNodeExprAutocomplete(editorPage)
    await expect(ac.option('false')).toBeVisible()
  })

  test('Autocomplete: Column methods', async ({ editorPage }) => {
    await prepare(editorPage)
    const ac = await getTableNodeExprAutocomplete(editorPage)
    await expect(ac.option('is_nan')).toBeVisible()
  })

  test('Autocomplete: Table columns', async ({ page, editorPage }) => {
    await prepare(editorPage)
    // Column data is requested asynchronously, and the menu options list is not reactive, so we
    // retry in case the menu is opened before the data has been received.
    await expect(async () => {
      await page.mouse.click(0, 0)
      const ac = await getTableNodeExprAutocomplete(editorPage)
      await expect(ac.option('Column A')).toBeVisible()
    }).toPass({ timeout: 5_000 })
  })

  async function getTableNodeExprAutocomplete(editorPage: EditorPageActions) {
    const node = editorPage.locateNodes('table')
    const exprText = node.locator('.WidgetText')
    await expect(exprText).toHaveAttribute('data-text-syntax', 'enso-table-expression')
    await exprText.click()
    await expect(exprText.getByTestId('widget-text-content')).toBeFocused()
    return await AutocompleteMenu.ForEditorInNode(node)
  }
})

class AutocompleteMenu {
  private constructor(private readonly root: Locator) {}

  static async ForEditorInNode(node: Locator): Promise<AutocompleteMenu> {
    const root = node.locator('.cm-tooltip-autocomplete')
    await expect(root).toBeVisible()
    return new AutocompleteMenu(root)
  }

  option(label: string) {
    return this.root.locator('.cm-completionLabel').filter({ hasText: label })
  }
}

test('Manage aggregates in `aggregate` node', async ({ editorPage, page }) => {
  await editorPage.mockMethodCallInfo('aggregated', {
    methodPointer: {
      module: 'Standard.Table.Table',
      definedOnType: 'Standard.Table.Table.Table',
      name: 'aggregate',
    },
    notAppliedArguments: [1, 2, 3],
  })

  // Hide docpanel to not obscure long node.
  await page.getByRole('tab', { name: 'Documentation' }).click()

  await editorPage
    // Check initially visible arguments
    .expectNodeTopLevelArgumentCount('aggregated', 1)
    .selectSingleNode('aggregated')
    // Check arguments after selecting node
    .expectNodeTopLevelArgumentCount('aggregated', 3)

  const node = editorPage.locateNodes('aggregated')
  // Add first aggregate
  const columnsArg = node
    .locator('.WidgetTopLevelArgument')
    .filter({ has: page.getByText('columns') })

  await locate.addItemButton(columnsArg).click()
  await editorPage.expectNodeTokens('aggregated', [
    'aggregate',
    'Aggregate_Column',
    '.',
    'Group_By',
  ])
  await editorPage.mockMethodCallInfo(
    { binding: 'aggregated', expr: 'Aggregate_Column.Group_By' },
    {
      methodPointer: {
        module: 'Standard.Table.Aggregate_Column',
        definedOnType: 'Standard.Table.Aggregate_Column.Aggregate_Column',
        name: 'Group_By',
      },
      notAppliedArguments: [0, 1],
    },
  )

  // Change aggregation type
  const columnsDropdown = new DropDownLocator(columnsArg)
  await columnsDropdown.clickWidget()
  await columnsDropdown.expectVisibleWithOptions(['Group_By', 'Count', 'Count_Distinct'])
  await columnsDropdown.clickOption('Count_Distinct')
  await expect(columnsArg.locator('.WidgetToken')).toContainText([
    'Aggregate_Column',
    '.',
    'Count_Distinct',
  ])
  await editorPage.mockMethodCallInfo(
    { binding: 'aggregated', expr: 'Aggregate_Column.Count_Distinct' },
    {
      methodPointer: {
        module: 'Standard.Table.Aggregate_Column',
        definedOnType: 'Standard.Table.Aggregate_Column.Aggregate_Column',
        name: 'Count_Distinct',
      },
      notAppliedArguments: [0, 1, 2],
    },
  )

  // Set column
  const firstItem = columnsArg
    .getByTestId('list-item-content')
    .locator('.WidgetPort > .WidgetSelection')
    .nth(0)
  const firstItemDropdown = new DropDownLocator(firstItem)
  await firstItemDropdown.clickWidget()
  await firstItemDropdown.expectVisibleWithOptions(['column 1', 'column 2'])
  await firstItemDropdown.clickOption('column 1')
  await expect(columnsArg.locator('.WidgetToken')).toContainText([
    'Aggregate_Column',
    '.',
    'Count_Distinct',
  ])
  await expect(columnsArg.getByTestId('widget-text-content').first()).toHaveText('column 1')

  // Add another aggregate
  await locate.addItemButton(columnsArg).click()
  await expect(columnsArg.locator('.WidgetToken')).toContainText([
    'Aggregate_Column',
    '.',
    'Count_Distinct',
    'Aggregate_Column',
    '.',
    'Group_By',
  ])
  await editorPage.mockMethodCallInfo(
    { binding: 'aggregated', expr: 'Aggregate_Column.Group_By' },
    {
      methodPointer: {
        module: 'Standard.Table.Aggregate_Column',
        definedOnType: 'Standard.Table.Aggregate_Column.Aggregate_Column',
        name: 'Group_By',
      },
      notAppliedArguments: [0, 1],
    },
  )

  // Set new aggregate's column
  const secondItem = columnsArg
    .getByTestId('list-item-content')
    .nth(1)
    .locator('.WidgetPort > .WidgetSelection')
  const secondItemDropdown = new DropDownLocator(secondItem)
  await secondItemDropdown.clickWidget()
  await secondItemDropdown.expectVisibleWithOptions(['column 1', 'column 2'])
  await secondItemDropdown.clickOption('column 2')
  await expect(secondItem.locator('.WidgetToken')).toContainText([
    'Aggregate_Column',
    '.',
    'Group_By',
  ])
  await expect(secondItem.getByTestId('widget-text-content').first()).toHaveText('column 2')

  // Switch aggregates
  //TODO[ao] I have no idea how to emulate drag. Simple dragTo does not work (some element seem to capture event).
  // When hovered, the handle becomes available after some time, but still mouse events don't have any effect.
  // I have no time now to investigate this.
  // Once fixed, add also removing element from vector here.

  // await columnsArg.locator('.item > .handle').nth(1).hover({ force: true })
  // await columnsArg.locator('.item > .handle').nth(1).hover()
  // await page.mouse.down()
  // await columnsArg.locator('.item > .handle').nth(0).hover({ force: true })
  // await columnsArg.locator('.item > .handle').nth(0).hover()
  // await page.mouse.up()
  // await expect(columnsArg.locator('.WidgetToken')).toContainText([
  //   'Aggregate_Column',
  //   '.',
  //   'Group_By',
  //   '"',
  //   'column 2',
  //   '"',
  //   'Aggregate_Column',
  //   '.',
  //   'Count_Distinct',
  //   '"',
  //   'column 1',
  //   '"',
  // ])
})

// Test that autoscoped constructors provide argument placeholders.
// This test can be removed when `aggregate` inserts autoscoped constructors by default,
// so this behavior will be tested in regular `aggregate` tests.
test('Autoscoped constructors', async ({ editorPage }) => {
  await editorPage
    .mockMethodCallInfo('autoscoped', {
      methodPointer: {
        module: 'Standard.Table.Table',
        definedOnType: 'Standard.Table.Table.Table',
        name: 'aggregate',
      },
      notAppliedArguments: [2, 3],
    })
    .mockMethodCallInfo(
      { binding: 'autoscoped', expr: '..Group_By' },
      {
        methodPointer: {
          module: 'Standard.Table.Aggregate_Column',
          definedOnType: 'Standard.Table.Aggregate_Column.Aggregate_Column',
          name: 'Group_By',
        },
        notAppliedArguments: [0, 1],
      },
    )
    .selectSingleNode('autoscoped')
    .withNode('autoscoped', async (node) => {
      const groupBy = node.getByTestId('list-item-content')
      await expect(groupBy).toBeVisible()
      await expect(groupBy.locator('.WidgetArgumentName')).toContainText(['column', 'as“”'])
    })
})

test('Table widget', async ({ editorPage, page }) => {
  await editorPage

  const node = await actions.createTableNode(page)
  const widget = node.locator('.WidgetTableEditor')
  await expect(widget).toBeVisible()
  await expect(widget.locator('.ag-header-cell-text')).toHaveText(['#'])
  await expect(widget.getByRole('button', { name: 'Add new column' })).toExist()
  await expect(widget.locator('.ag-cell')).toHaveText(['0', ''])

  // Create first column
  await widget.getByRole('button', { name: 'Add new column' }).click()
  await expect(widget.locator('.ag-header-cell-text')).toHaveText(['#', 'Column 1'])
  await expect(widget.locator('.ag-cell')).toHaveText(['0', '', ''])

  // Putting first value
  await widget.locator('.ag-cell', { hasNotText: '0' }).first().click()
  await page.keyboard.type('Value')
  await page.keyboard.press('Enter')
  // There will be new blank row allowing adding new rows.
  await expect(widget.locator('.ag-cell')).toHaveText(['0', 'Value', '', '1', '', ''])

  // Renaming column
  await widget.locator('.ag-header-cell-text', { hasText: 'Column 1' }).first().click()
  await page.keyboard.type('Header')
  await page.keyboard.press('Enter')
  await expect(widget.locator('.ag-header-cell-text')).toHaveText(['#', 'Header'])

  // Adding next column
  await widget.getByRole('button', { name: 'Add new column' }).click()
  await expect(widget.locator('.ag-header-cell-text')).toHaveText(['#', 'Header', 'Column 2'])
  await expect(widget.locator('.ag-cell')).toHaveText(['0', 'Value', '', '', '1', '', '', ''])

  // Switching edit between cells and headers - check we will never edit two things at once.
  await expect(widget.locator('.ag-text-field-input')).toHaveCount(0)
  await widget.locator('.ag-header-cell-text', { hasNotText: /#/ }).first().click()
  await expect(widget.locator('.ag-text-field-input')).toHaveCount(1)
  await widget.locator('.valueCell').first().dblclick()
  await expect(widget.locator('.ag-text-field-input')).toHaveCount(1)
  await widget.locator('.ag-header-cell-text', { hasNotText: /#/ }).first().click()
  await expect(widget.locator('.ag-text-field-input')).toHaveCount(1)
  // The header after click stops editing immediately. Tracked by #11150
  // await widget.locator('.ag-header-cell-text', { hasNotText: /#/ }).last().dblclick()
  // await expect(widget.locator('.ag-text-field-input')).toHaveCount(1)
  await page.keyboard.press('Escape')
  await expect(widget.locator('.ag-text-field-input')).toHaveCount(0)
})

test('Text widget can be refocused after focus is lost in an unexpected way (#12571)', async ({
  editorPage,
  page,
}) => {
  await editorPage
  const textNode = locate.graphNodeByBinding(page, 'text')
  const textInput = textNode.getByTestId('widget-text-content')
  await textInput.click()
  await expect(textInput).toBeFocused()
  await page.evaluate(() => (document.activeElement! as HTMLElement).blur())
  await expect(textInput).not.toBeFocused()
  await textInput.click()
  await expect(textInput).toBeFocused()
})
