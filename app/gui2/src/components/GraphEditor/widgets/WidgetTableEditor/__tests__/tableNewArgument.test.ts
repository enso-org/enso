import { useTableNewArgument } from '@/components/GraphEditor/widgets/WidgetTableEditor/tableNewArgument'
import { WidgetInput } from '@/providers/widgetRegistry'
import { Ast } from 'shared/ast'
import { expect, test } from 'vitest'

test.each([
  {
    code: 'Table.new [["a", [1, 2, 3]], ["b", [4, 5, "six"]]]',
    expectedColumnDefs: [{ field: 'a' }, { field: 'b' }],
    expectedRowData: [
      { a: 1, b: 4 },
      { a: 2, b: 5 },
      { a: 3, b: 'six' },
    ],
  },
  {
    code: 'Table.new []',
    expectedColumnDefs: [],
    expectedRowData: [],
  },
  {
    code: 'Table.new [["a", []]]',
    expectedColumnDefs: [{ field: 'a' }],
    expectedRowData: [],
  },
])('Reading table from $code', ({ code, expectedColumnDefs, expectedRowData }) => {
  const ast = Ast.parse(code)
  const input = WidgetInput.FromAst(ast)
  const tableNewArgs = useTableNewArgument(input)
  expect(tableNewArgs.columnDefs.value).toEqual(
    Array.from(expectedColumnDefs, (colDef) => expect.objectContaining(colDef)),
  )
  expect(tableNewArgs.rowData.value).toEqual(expectedRowData)
})
