import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { transpose } from 'enso-common/src/utilities/data/array'
import * as iter from 'enso-common/src/utilities/data/iter'
import Papa from 'papaparse'
import { computed } from 'vue'
import { DEFAULT_COLUMN_PREFIX, NOTHING_NAME } from './tableInputArgument'

const toTable = computed(() => Pattern.parseExpression('Table.input __'))

/**
 * Parse data in TSV format (according to RFC 4180).
 * @throws if the number of columns in each row is not the same.
 * @returns an array of rows, each row is an array of cells.
 */
function parseTsvDataImpl(tsvData: string): string[][] {
  const parseResult = Papa.parse(tsvData, { delimiter: '\t', header: false })
  for (const error of parseResult.errors) {
    // These errors not necessearily mean that the parsing failed.
    // Malformed TSV data is a non-existent beast (?).
    console.error('Error parsing TSV data:', error)
  }
  // The conversion is safe according to the documentation of Papa.parse, as we set `header: false`.
  const rows = parseResult.data as string[][]
  for (const row of rows) {
    if (row.length !== rows[0]!.length) {
      throw new Error('All rows must have the same number of columns.')
    }
  }
  return rows
}

/**
 * Parse data in TSV format (according to RFC 4180). Each row has the same number of cells.
 * @returns an array of rows, each row is an array of cells, or null if the parsing failed.
 */
export function parseTsvData(tsvData: string): string[][] | null {
  try {
    return parseTsvDataImpl(tsvData)
  } catch (error) {
    console.error('Failed to parse TSV data:', error)
    return null
  }
}

/** Serialize rows to TSV format. The reverse of {@link parseTsvData}. */
export function rowsToTsv(rows: string[][]): string {
  return Papa.unparse(rows, { delimiter: '\t', newline: '\r\n' })
}

/**
 * Create `Table.input` expression generating table from the provided rows.
 * @param rows - String values to be inserted into the table.
 * @param columnNames - Optional column names to be used in the resulting table.
 *   If not provided, default column names are generated.
 */
export function tableToEnsoExpression(rows: string[][], columnNames?: string[]): string | null {
  const table = transpose(rows)
  const getColumnName = (index: number) => {
    if (columnNames && columnNames[index]) return columnNames[index]
    return `${DEFAULT_COLUMN_PREFIX}${index + 1}`
  }
  const emptyCell = (module: Ast.MutableModule) => Ast.Ident.new(module, NOTHING_NAME)
  const columnAst = Ast.Vector.tryBuild(iter.enumerate(table), ([column, index], module) => {
    const columnName = Ast.TextLiteral.new(getColumnName(index), module)
    const makeCell = (cell: string, module: Ast.MutableModule): Ast.Owned<Ast.MutableAst> =>
      cell === '' ? emptyCell(module) : Ast.TextLiteral.new(cell, module)
    const values = Ast.Vector.tryBuild(column, makeCell, module)
    return Ast.Vector.new(module, [columnName, values])
  })
  return toTable.value.instantiate(columnAst.module, [columnAst]).code()
}
