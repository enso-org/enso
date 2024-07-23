import type { WidgetInput } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { filterDefined } from '@/util/data/iterable'
import type { ToValue } from '@/util/reactivity'
import { mapIterator } from 'lib0/iterator'
import { initializeFFI } from 'shared/ast/ffi'
import { computed, toValue } from 'vue'

await initializeFFI()

export function useTableNewArgument(input: ToValue<WidgetInput & { value: Ast.Ast }>) {
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
    return Array.from(columns.value, (col) => ({
      field: col.name,
      editable: true,
    }))
  })

  const rowData = computed(() => {
    const cols = Array.from(columns.value, (col) => ({ name: col.name, data: col.data.values() }))
    const rows: Record<string, any>[] = []
    let anyValue: boolean
    do {
      const row: Record<string, any> = {}
      anyValue = false
      for (const col of cols) {
        const valueAst = col.data.next()
        if (valueAst.done) continue
        const value =
          valueAst.value instanceof Ast.TextLiteral ? valueAst.value.rawTextContent
          : valueAst.value instanceof Ast.NumericLiteral ? parseFloat(valueAst.value.code())
          : undefined
        if (value == null) continue
        anyValue = true
        row[col.name] = value
      }
      if (anyValue) {
        rows.push(row)
      }
    } while (anyValue)
    return rows
  })

  return { columnDefs, rowData }
}
