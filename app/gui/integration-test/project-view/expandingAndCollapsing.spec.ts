import { expect, test } from 'integration-test/base'
import * as actions from 'integration-test/project-view/actions'

test('Collapsing and expanding Table.input node', async ({ editorPage, page }) => {
  await editorPage

  const tableNode = await actions.createTableNode(page, '[["A", ["1", "2", "3"]]]')
  await expect(tableNode.locator('.ag-cell').first()).toBeVisible()
  await tableNode.getByTestId('more-button').click()
  await tableNode.getByRole('button', { name: 'Collapse Component' }).click()

  await expect(tableNode.locator('.ag-cell')).toHaveCount(0)
  await expect(tableNode.locator('.content')).toHaveText('Table (3 rows)')

  await tableNode.getByTestId('more-button').click()
  await tableNode.getByRole('button', { name: 'Expand Component' }).click()
  await expect(tableNode.locator('.ag-cell').first()).toBeVisible()
})
