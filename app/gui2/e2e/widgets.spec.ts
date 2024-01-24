import assert from 'assert'
import test, { expect, type Locator, type Page } from 'playwright/test'
import * as Ast from 'src/util/ast/abstract'
import * as actions from './actions'
import * as customExpect from './customExpect'
import { mockExpressionUpdate, mockMethodCallInfo } from './expressionUpdates'
import * as locate from './locate'

class DropDownLocator {
  readonly dropDown: Locator
  readonly items: Locator

  constructor(page: Page) {
    this.dropDown = page.locator('.dropdownContainer')
    this.items = this.dropDown.locator('.selectable-item')
  }

  async expectVisibleWithOptions(page: Page, options: string[]): Promise<void> {
    await expect(this.dropDown).toBeVisible()
    await expect(this.items).toHaveCount(options.length)
    for (const option of options) {
      await expect(this.items.filter({ has: page.getByText(option) })).toBeVisible()
    }
  }

  async clickOption(page: Page, option: string): Promise<void> {
    await this.items.filter({ has: page.getByText(option) }).click()
  }
}

test('Selection widgets in Data.read node', async ({ page }) => {
  await actions.goToGraph(page)
  await mockMethodCallInfo(page, 'data', {
    methodPointer: {
      module: 'Standard.Base.Data',
      definedOnType: 'Standard.Base.Data',
      name: 'read',
    },
    notAppliedArguments: [0, 1, 2],
  })

  const dropDown = new DropDownLocator(page)

  // Check initially visible arguments
  const node = locate.graphNodeByBinding(page, 'data')
  const argumentNames = node.locator('.WidgetArgumentName')
  await expect(argumentNames).toHaveCount(3)

  // Set value on `on_problems` (static drop-down)
  const onProblemsArg = argumentNames.filter({ has: page.getByText('on_problems') })
  await onProblemsArg.click()
  await dropDown.expectVisibleWithOptions(page, ['Ignore', 'Report Warning', 'Report Error'])
  await dropDown.clickOption(page, 'Report Error')
  await expect(onProblemsArg.locator('.WidgetToken')).toHaveText([
    'Problem_Behavior',
    '.',
    'Report_Error',
  ])

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
  await dropDown.expectVisibleWithOptions(page, ['Ignore', 'Report Warning', 'Report Error'])
  await dropDown.clickOption(page, 'Report Warning')
  await expect(onProblemsArg.locator('.WidgetToken')).toHaveText([
    'Problem_Behavior',
    '.',
    'Report_Warning',
  ])

  // Set value on `path` (dynamic config)
  const pathArg = argumentNames.filter({ has: page.getByText('path') })
  await pathArg.click()
  await expect(page.locator('.dropdownContainer')).toBeVisible()
  await dropDown.expectVisibleWithOptions(page, ['File 1', 'File 2'])
  await dropDown.clickOption(page, 'File 2')
  await expect(pathArg.locator('.WidgetToken')).toHaveText(['"', 'File 2', '"'])

  // Change value on `path` (dynamic config)
  await mockMethodCallInfo(page, 'data', {
    methodPointer: {
      module: 'Standard.Base.Data',
      definedOnType: 'Standard.Base.Data',
      name: 'read',
    },
    notAppliedArguments: [1],
  })
  await page.getByText('File 2').click()
  await dropDown.expectVisibleWithOptions(page, ['File 1', 'File 2'])
  await dropDown.clickOption(page, 'File 1')
  await expect(pathArg.locator('.WidgetToken')).toHaveText(['"', 'File 1', '"'])
})

test('Managing aggregates in `aggregate` node', async ({ page }) => {
  await actions.goToGraph(page)
  await mockMethodCallInfo(page, 'aggregated', {
    methodPointer: {
      module: 'Standard.Table.Data.Table',
      definedOnType: 'Standard.Table.Data.Table.Table',
      name: 'aggregate',
    },
    notAppliedArguments: [1, 2, 3],
  })
  const dropDown = new DropDownLocator(page)

  // Check initially visible arguments
  const node = locate.graphNodeByBinding(page, 'aggregated')
  const argumentNames = node.locator('.WidgetArgumentName')
  await expect(argumentNames).toHaveCount(3)

  // Add first column
  const columnsArg = argumentNames.filter({ has: page.getByText('columns') })
  await columnsArg.locator('.add-item').click()
  await expect(columnsArg.locator('.WidgetToken')).toHaveText(['Aggregate_Column', '.', 'Group_By'])
  await mockMethodCallInfo(
    page,
    {
      binding: 'aggregated',
      expr: 'Aggregate_Column.Group_By',
    },
    {
      methodPointer: {
        module: 'Standard.Table.Data.Aggregate_Column',
        definedOnType: 'Standard.Table.Data.Aggregate_Column.Aggregate_Column',
        name: 'Group_By',
      },
      notAppliedArguments: [0],
    },
  )

  // Set column
  const columnArg = columnsArg.locator('.WidgetSelection .WidgetSelection')
  await columnArg.click()
  await dropDown.expectVisibleWithOptions(page, ['column 1', 'column 2'])
})
