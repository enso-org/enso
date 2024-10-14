import {
  MenuItem,
  RowData,
  tableNewCallMayBeHandled,
  useTableNewArgument,
} from '@/components/GraphEditor/widgets/WidgetTableEditor/tableNewArgument'
import { WidgetInput } from '@/providers/widgetRegistry'
import { SuggestionDb } from '@/stores/suggestionDatabase'
import { makeType } from '@/stores/suggestionDatabase/entry'
import { assert } from '@/util/assert'
import { Ast } from '@/util/ast'
import { GetContextMenuItems, GetMainMenuItems } from 'ag-grid-enterprise'
import { expect, test, vi } from 'vitest'

function suggestionDbWithNothing() {
  const db = new SuggestionDb()
  db.set(1, makeType('Standard.Base.Nothing.Nothing'))
  return db
}

test.each([
  {
    code: 'Table.new [["a", [1, 2, 3]], ["b", [4, 5, "six"]], ["empty", [Nothing, Standard.Base.Nothing, Nothing]]]',
    expectedColumnDefs: [
      { headerName: '#' },
      { headerName: 'a' },
      { headerName: 'b' },
      { headerName: 'empty' },
      { headerName: 'New Column' },
    ],
    expectedRows: [
      { '#': 0, a: 1, b: 4, empty: null, 'New Column': null },
      { '#': 1, a: 2, b: 5, empty: null, 'New Column': null },
      { '#': 2, a: 3, b: 'six', empty: null, 'New Column': null },
      { '#': 3, a: null, b: null, empty: null, 'New Column': null },
    ],
  },
  {
    code: 'Table.new []',
    expectedColumnDefs: [{ headerName: '#' }, { headerName: 'New Column' }],
    expectedRows: [{ '#': 0, 'New Column': null }],
  },
  {
    code: 'Table.new',
    expectedColumnDefs: [{ headerName: '#' }, { headerName: 'New Column' }],
    expectedRows: [{ '#': 0, 'New Column': null }],
  },
  {
    code: 'Table.new _',
    expectedColumnDefs: [{ headerName: '#' }, { headerName: 'New Column' }],
    expectedRows: [{ '#': 0, 'New Column': null }],
  },
  {
    code: 'Table.new [["a", []]]',
    expectedColumnDefs: [{ headerName: '#' }, { headerName: 'a' }, { headerName: 'New Column' }],
    expectedRows: [{ '#': 0, a: null, 'New Column': null }],
  },
  {
    code: 'Table.new [["a", [1,,2]], ["b", [3, 4,]], ["c", [, 5, 6]], ["d", [,,]]]',
    expectedColumnDefs: [
      { headerName: '#' },
      { headerName: 'a' },
      { headerName: 'b' },
      { headerName: 'c' },
      { headerName: 'd' },
      { headerName: 'New Column' },
    ],
    expectedRows: [
      { '#': 0, a: 1, b: 3, c: null, d: null, 'New Column': null },
      { '#': 1, a: null, b: 4, c: 5, d: null, 'New Column': null },
      { '#': 2, a: 2, b: null, c: 6, d: null, 'New Column': null },
      { '#': 3, a: null, b: null, c: null, d: null, 'New Column': null },
    ],
  },
])('Reading table from $code', ({ code, expectedColumnDefs, expectedRows }) => {
  const ast = Ast.parse(code)
  expect(tableNewCallMayBeHandled(ast)).toBeTruthy()
  const input = WidgetInput.FromAst(ast)
  const startEdit = vi.fn()
  const addMissingImports = vi.fn()
  const onUpdate = vi.fn()
  const tableNewArgs = useTableNewArgument(
    input,
    { startEdit, addMissingImports },
    suggestionDbWithNothing(),
    onUpdate,
  )
  expect(tableNewArgs.columnDefs.value).toEqual(
    Array.from(expectedColumnDefs, (colDef) => expect.objectContaining(colDef)),
  )
  const resolvedRow = Array.from(tableNewArgs.rowData.value, (row) =>
    Object.fromEntries(
      tableNewArgs.columnDefs.value.map((col) => [col.headerName, col.valueGetter({ data: row })]),
    ),
  )
  expect(resolvedRow).toEqual(expectedRows)

  function* expectedIndices() {
    for (let i = 0; i < expectedRows.length; ++i) {
      yield expect.objectContaining({ index: i })
    }
  }
  expect(tableNewArgs.rowData.value).toEqual([...expectedIndices()])
  expect(startEdit).not.toHaveBeenCalled()
  expect(onUpdate).not.toHaveBeenCalled()
  expect(addMissingImports).not.toHaveBeenCalled()
})

test.each([
  'Table.new 14',
  'Table.new array1',
  "Table.new ['a', [123]]",
  "Table.new [['a', [123]], ['b', [124], []]]",
  "Table.new [['a', [123]], ['a'.repeat 170, [123]]]",
  "Table.new [['a', [1, 2, 3, 3 + 1]]]",
])('"%s" is not valid input for Table Editor Widget', (code) => {
  const ast = Ast.parse(code)
  expect(tableNewCallMayBeHandled(ast)).toBeFalsy()
})

test.each([
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    description: 'Editing number',
    edit: { column: 1, row: 1, value: -22 },
    expected: "Table.new [['a', [1, -22, 3]], ['b', [4, 5, 6]]]",
  },
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    description: 'Editing string',
    edit: { column: 1, row: 1, value: 'two' },
    expected: "Table.new [['a', [1, 'two', 3]], ['b', [4, 5, 6]]]",
  },
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    description: 'Putting blank value',
    edit: { column: 2, row: 1, value: '' },
    expected: "Table.new [['a', [1, 2, 3]], ['b', [4, Nothing, 6]]]",
    importExpected: true,
  },
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    description: 'Adding new column',
    edit: { column: 3, row: 1, value: 8 },
    expected:
      "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]], ['New Column', [Nothing, 8, Nothing]]]",
    importExpected: true,
  },
  {
    code: 'Table.new []',
    description: 'Adding first column',
    edit: { column: 1, row: 0, value: 8 },
    expected: "Table.new [['New Column', [8]]]",
  },
  {
    code: 'Table.new',
    description: 'Adding parameter',
    edit: { column: 1, row: 0, value: 8 },
    expected: "Table.new [['New Column', [8]]]",
  },
  {
    code: 'Table.new _',
    description: 'Update parameter',
    edit: { column: 1, row: 0, value: 8 },
    expected: "Table.new [['New Column', [8]]]",
  },
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    description: 'Adding new row',
    edit: { column: 1, row: 3, value: 4.5 },
    expected: "Table.new [['a', [1, 2, 3, 4.5]], ['b', [4, 5, 6, Nothing]]]",
    importExpected: true,
  },
  {
    code: "Table.new [['a', []], ['b', []]]",
    description: 'Adding first row',
    edit: { column: 2, row: 0, value: 'val' },
    expected: "Table.new [['a', [Nothing]], ['b', ['val']]]",
    importExpected: true,
  },
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    description: 'Adding new row and column (the cell in the corner)',
    edit: { column: 3, row: 3, value: 7 },
    expected:
      "Table.new [['a', [1, 2, 3, Nothing]], ['b', [4, 5, 6, Nothing]], ['New Column', [Nothing, Nothing, Nothing, 7]]]",
    importExpected: true,
  },
  {
    code: "Table.new [['a', [1, ,3]]]",
    description: 'Setting missing value',
    edit: { column: 1, row: 1, value: 2 },
    expected: "Table.new [['a', [1, 2 ,3]]]",
  },
  {
    code: "Table.new [['a', [, 2, 3]]]",
    description: 'Setting missing value at first row',
    edit: { column: 1, row: 0, value: 1 },
    expected: "Table.new [['a', [1, 2, 3]]]",
  },
  {
    code: "Table.new [['a', [1, 2,]]]",
    description: 'Setting missing value at last row',
    edit: { column: 1, row: 2, value: 3 },
    expected: "Table.new [['a', [1, 2, 3]]]",
  },
  {
    code: "Table.new [['a', [1, 2]], ['a', [3, 4]]]",
    description: 'Editing with duplicated column name',
    edit: { column: 1, row: 1, value: 5 },
    expected: "Table.new [['a', [1, 5]], ['a', [3, 4]]]",
  },
])('Editing table $code: $description', ({ code, edit, expected, importExpected }) => {
  const ast = Ast.parseBlock(code)
  const inputAst = [...ast.statements()][0]
  assert(inputAst != null)
  const input = WidgetInput.FromAst(inputAst)
  const onUpdate = vi.fn((update) => {
    const inputAst = [...update.edit.getVersion(ast).statements()][0]
    expect(inputAst?.code()).toBe(expected)
  })
  const addMissingImports = vi.fn((_, imports) => {
    expect(imports).toEqual([
      {
        kind: 'Unqualified',
        from: 'Standard.Base.Nothing',
        import: 'Nothing',
      },
    ])
  })
  const tableNewArgs = useTableNewArgument(
    input,
    { startEdit: () => ast.module.edit(), addMissingImports },
    suggestionDbWithNothing(),
    onUpdate,
  )
  const editedRow = tableNewArgs.rowData.value[edit.row]
  assert(editedRow != null)
  tableNewArgs.columnDefs.value[edit.column]?.valueSetter?.({
    data: editedRow,
    newValue: edit.value,
  })
  expect(onUpdate).toHaveBeenCalledOnce()
  if (importExpected) expect(addMissingImports).toHaveBeenCalled()
  else expect(addMissingImports).not.toHaveBeenCalled()
})

function getCustomMenuItemByName(
  name: string,
  items:
    | (string | MenuItem)[]
    | GetMainMenuItems<RowData>
    | GetContextMenuItems<RowData>
    | undefined,
): MenuItem | undefined {
  if (!(items instanceof Array)) return undefined
  const found = items.find((item) => typeof item === 'object' && item.name === name)
  return typeof found === 'object' ? found : undefined
}

test.each([
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    removedRowIndex: 0,
    expected: "Table.new [['a', [ 2, 3]], ['b', [ 5, 6]]]",
  },
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    removedRowIndex: 1,
    expected: "Table.new [['a', [1, 3]], ['b', [4, 6]]]",
  },
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    removedRowIndex: 2,
    expected: "Table.new [['a', [1, 2]], ['b', [4, 5]]]",
  },
  {
    code: "Table.new [['a', [1]], ['b', [4]]]",
    removedRowIndex: 0,
    expected: "Table.new [['a', []], ['b', []]]",
  },
])('Removing $removedRowIndex row in $code', ({ code, removedRowIndex, expected }) => {
  const ast = Ast.parseBlock(code)
  const inputAst = [...ast.statements()][0]
  assert(inputAst != null)
  const addMissingImports = vi.fn()
  const onUpdate = vi.fn((update) => {
    const inputAst = [...update.edit.getVersion(ast).statements()][0]
    expect(inputAst?.code()).toBe(expected)
  })
  const input = WidgetInput.FromAst(inputAst)
  const tableNewArgs = useTableNewArgument(
    input,
    { startEdit: () => ast.module.edit(), addMissingImports },
    new SuggestionDb(),
    onUpdate,
  )
  const removedRow = tableNewArgs.rowData.value[removedRowIndex]
  assert(removedRow != null)
  // Context menu of all cells in given row should work (even the "virtual" columns).
  for (const colDef of tableNewArgs.columnDefs.value) {
    const removeAction = getCustomMenuItemByName('Remove Row', colDef.contextMenuItems)
    assert(removeAction != null)
    removeAction.action({ node: { data: removedRow } })
    expect(onUpdate).toHaveBeenCalledOnce()
    onUpdate.mockClear()
  }
  expect(addMissingImports).not.toHaveBeenCalled()
})

test.each([
  {
    code: "Table.new [['a', [1, 2]], ['b', [3, 4]], ['c', [5, 6]]]",
    removedColIndex: 1,
    expected: "Table.new [ ['b', [3, 4]], ['c', [5, 6]]]",
  },
  {
    code: "Table.new [['a', [1, 2]], ['b', [3, 4]], ['c', [5, 6]]]",
    removedColIndex: 2,
    expected: "Table.new [['a', [1, 2]], ['c', [5, 6]]]",
  },
  {
    code: "Table.new [['a', [1, 2]], ['b', [3, 4]], ['c', [5, 6]]]",
    removedColIndex: 3,
    expected: "Table.new [['a', [1, 2]], ['b', [3, 4]]]",
  },
  {
    code: "Table.new [['a', [1, 2]]]",
    removedColIndex: 1,
    expected: 'Table.new []',
  },
])('Removing $removedColIndex column in $code', ({ code, removedColIndex, expected }) => {
  const ast = Ast.parseBlock(code)
  const inputAst = [...ast.statements()][0]
  assert(inputAst != null)
  const addMissingImports = vi.fn()
  const onUpdate = vi.fn((update) => {
    const inputAst = [...update.edit.getVersion(ast).statements()][0]
    expect(inputAst?.code()).toBe(expected)
  })
  const input = WidgetInput.FromAst(inputAst)
  const tableNewArgs = useTableNewArgument(
    input,
    { startEdit: () => ast.module.edit(), addMissingImports },
    new SuggestionDb(),
    onUpdate,
  )
  const removedCol = tableNewArgs.columnDefs.value[removedColIndex]
  assert(removedCol != null)
  const removeAction = getCustomMenuItemByName('Remove Column', removedCol.mainMenuItems)
  assert(removeAction != null)
  removeAction.action({ node: null })
  expect(onUpdate).toHaveBeenCalledOnce()
  onUpdate.mockClear()

  const cellRemoveAction = getCustomMenuItemByName('Remove Column', removedCol.contextMenuItems)
  cellRemoveAction?.action({ node: { data: tableNewArgs.rowData.value[0] } })
  expect(onUpdate).toHaveBeenCalledOnce()

  expect(addMissingImports).not.toHaveBeenCalled()
})
