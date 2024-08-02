import type { WidgetInput, WidgetUpdate } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { filterDefined } from '@/util/data/iterable'
import type { ToValue } from '@/util/reactivity'
import { type ColDef } from 'ag-grid-community'
import { mapIterator } from 'lib0/iterator'
import type { AstId, MutableModule } from 'shared/ast'
import { initializeFFI } from 'shared/ast/ffi'
import { assert } from 'shared/util/assert'
import { computed, toValue } from 'vue'

await initializeFFI()

export type RowData = {
  index: number
  /* Column id to given row's cell id. */
  cells: Record<AstId, AstId>
}

/** A more specialized version of AGGrid's `ColDef` to simplify testing (the tests need to provide
 * only values actually used by the composable) */
interface ColumnDef {
  colId?: string
  headerName: string
  valueGetter: ({ data }: { data: RowData | undefined }) => any
  valueSetter: ({ data, newValue }: { data: RowData; newValue: any }) => boolean
}

namespace cellValueConversion {
  export function astToAgGrid(ast: Ast.Ast) {
    if (ast instanceof Ast.TextLiteral) return ast.rawTextContent
    else if (ast instanceof Ast.NumericLiteral) return parseFloat(ast.code())
    else return undefined
  }

  export function agGridToAst(value: unknown, module: MutableModule): Ast.Owned {
    if (value == null) {
      return Ast.TextLiteral.new('', module)
    }
    return (
      Ast.NumericLiteral.tryParse(`${value}`, module) ?? Ast.TextLiteral.new(`${value}`, module)
    )
  }
}

export function useTableNewArgument(
  input: ToValue<WidgetInput & { value: Ast.Ast }>,
  graph: { startEdit(): Ast.MutableModule },
  onUpdate: (update: WidgetUpdate) => void,
) {
  const columnsAst = computed(() => {
    const inputAst = toValue(input).value
    if (!(inputAst instanceof Ast.App)) return undefined
    if (!(inputAst.argument instanceof Ast.Vector)) return undefined
    return inputAst.argument
  })

  const columns = computed(() => {
    const cols = columnsAst.value?.values()
    if (cols == null) return []
    const arr = Array.from(filterDefined(mapIterator(cols, (element) => readColumn(element))))
    if (arr.some((x) => x == null)) return []
    return arr
  })

  const rowCount = computed(() =>
    columns.value.reduce((soFar, col) => Math.max(soFar, col.data.length), 0),
  )

  const undersizedColumns = computed(() =>
    columns.value.filter((col) => col.data.length < rowCount.value),
  )

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

  function fixColumns(edit: Ast.MutableModule) {
    for (const column of undersizedColumns.value) {
      const data = edit.getVersion(column.data)
      while (data.length < rowCount.value) {
        data.push(Ast.TextLiteral.new('', edit))
      }
      while (data.length > rowCount.value) {
        data.pop()
      }
    }
  }

  function addRow(edit: MutableModule, columnWithValue?: Ast.AstId, value?: unknown) {
    for (const column of columns.value) {
      const editedCol = edit.getVersion(column.data)
      if (column.data.id === columnWithValue) {
        editedCol.push(cellValueConversion.agGridToAst(value, edit))
      } else {
        editedCol.push(Ast.TextLiteral.new('', edit))
      }
    }
  }

  function addColumn(edit: MutableModule, rowWithValue: number, value: unknown) {
    const newColumnSize = Math.max(rowCount.value, rowWithValue + 1)
    function* cellsGenerator() {
      for (let i = 0; i < newColumnSize; ++i) {
        if (i === rowWithValue) yield cellValueConversion.agGridToAst(value, edit)
        else yield cellValueConversion.agGridToAst(undefined, edit)
      }
    }
    const cells = Ast.Vector.new(edit, Array.from(cellsGenerator()))
    const newCol = Ast.Vector.new(edit, [Ast.TextLiteral.new('New Column'), cells])
    if (columnsAst.value) {
      edit.getVersion(columnsAst.value).push(newCol)
    } else {
      const inputAst = edit.getVersion(toValue(input).value)
      const newArg = Ast.Vector.new(edit, [newCol])
      if (inputAst instanceof Ast.MutableApp) {
        inputAst.setArgument(newArg)
      } else {
        inputAst.updateValue((func) => Ast.App.new(edit, func, undefined, newArg))
      }
    }
  }

  const newColumnDef = computed<ColumnDef>(
    () =>
      ({
        headerName: 'New Column',
        valueGetter: () => undefined,
        valueSetter: ({ data, newValue }: { data: RowData; newValue: any }) => {
          const edit = graph.startEdit()
          if (data.index === rowCount.value) {
            addRow(edit)
          }
          addColumn(edit, data.index, newValue)
          onUpdate({ edit })
          return true
        },
      }) satisfies ColDef<RowData>,
  )

  const columnDefs = computed(() => {
    const cols: ColumnDef[] = Array.from(
      columns.value,
      (col) =>
        ({
          colId: col.data.id,
          headerName: col.name,
          valueGetter: ({ data }: { data: RowData | undefined }) => {
            if (data == null) return undefined
            const ast = toValue(input).value.module.tryGet(data.cells[col.data.id])
            if (ast == null) return undefined
            return cellValueConversion.astToAgGrid(ast)
          },
          valueSetter: ({ data, newValue }: { data: RowData; newValue: any }): boolean => {
            const astId = data?.cells[col.data.id]
            const edit = graph.startEdit()
            fixColumns(edit)
            if (data.index === rowCount.value) {
              addRow(edit, col.data.id, newValue)
            } else {
              const newValueAst = cellValueConversion.agGridToAst(newValue, edit)
              if (astId != null) edit.replaceValue(astId, newValueAst)
              else edit.getVersion(col.data).set(data.index, newValueAst)
            }
            onUpdate({ edit })
            return true
          },
        }) satisfies ColDef<RowData>,
    )
    cols.push(newColumnDef.value)
    return cols
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
          row.cells[col.data.id] = value?.id
        }
      }
    }
    rows.push({ index: rows.length, cells: {} })
    return rows
  })

  return { columnDefs, rowData }
}
