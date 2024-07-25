import type { WidgetInput } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { filterDefined } from '@/util/data/iterable'
import type { ToValue } from '@/util/reactivity'
import type { ValueGetterParams, ValueSetterParams } from 'ag-grid-community'
import { mapIterator } from 'lib0/iterator'
import type { AstId } from 'shared/ast'
import { initializeFFI } from 'shared/ast/ffi'
import { assert } from 'shared/util/assert'
import { computed, toValue } from 'vue'

await initializeFFI()

export type RowData = {
  index: number
  cells: Record<string, AstId>
}

export function useTableNewArgument(
  input: ToValue<WidgetInput & { value: Ast.Ast }>,
  onChange: (id: AstId, newValue: string) => boolean,
) {
  const columnsAst = computed(() => {
    const inputAst = toValue(input).value
    if (!(inputAst instanceof Ast.App)) return undefined
    if (!(inputAst.argument instanceof Ast.Vector)) return undefined
    console.log('new columnsAst')
    return inputAst.argument
  })

  const columns = computed(() => {
    const cols = columnsAst.value?.values()
    if (cols == null) return []
    const arr = Array.from(filterDefined(mapIterator(cols, (element) => readColumn(element))))
    if (arr.some((x) => x == null)) return []
    console.log('new columns')
    return arr
  })

  function readColumn(ast: Ast.Ast): { name: string; data: Ast.Vector } | undefined {
    if (!(ast instanceof Ast.Vector)) return undefined
    const elements = ast.values()
    const first = elements.next()
    if (first.done) return undefined
    const second = elements.next()
    if (second.done) return undefined
    if (!elements.next().done) return undefined

    if (!(first.value instanceof Ast.TextLiteral)) return undefined
    if (!(second.value instanceof Ast.Vector)) return undefined
    return { name: first.value.rawTextContent, data: second.value }
  }

  const columnDefs = computed(() => {
    const inputVal = toValue(input)
    console.log('new columnDefs')
    return Array.from(columns.value, (col) => ({
      headerName: col.name,
      editable: true,
      valueGetter: ({ data }: ValueGetterParams<RowData>) =>
        data ? inputVal.value.module.get(data.cells[col.name])?.code() : undefined,
      valueSetter: ({ data, newValue }: ValueSetterParams<RowData>) => {
        const astId = data?.cells[col.name]
        if (astId != null) onChange(astId, newValue)
      },
    }))
  })

  const rowData = computed(() => {
    const rows: RowData[] = []
    for (const col of columns.value) {
      for (const [rowIndex, value] of col.data.enumerate()) {
        const row: RowData = rows.at(rowIndex) ?? { index: rowIndex, cells: {} }
        assert(rowIndex <= rows.length)
        if (rowIndex === rows.length) {
          rows.push(row)
        }
        if (value?.id) {
          row.cells[col.name] = value?.id
        }
      }
    }
    console.log('new rows')
    return rows
  })

  return { columnDefs, rowData }
}
