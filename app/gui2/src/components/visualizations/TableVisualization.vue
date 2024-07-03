<script lang="ts">
import icons from '@/assets/icons.svg'
import {
  clipboardNodeData,
  tsvTableToEnsoExpression,
  writeClipboard,
} from '@/components/GraphEditor/clipboard'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { useAutoBlur } from '@/util/autoBlur'
import { VisualizationContainer, useVisualizationConfig } from '@/util/visualizationBuiltins'
import '@ag-grid-community/styles/ag-grid.css'
import '@ag-grid-community/styles/ag-theme-alpine.css'
import type {
  CellClassParams,
  CellClickedEvent,
  ColumnResizedEvent,
  ICellRendererParams,
} from 'ag-grid-community'
import type { ColDef, GridOptions, HeaderValueGetterParams } from 'ag-grid-enterprise'
import {
  computed,
  onMounted,
  onUnmounted,
  reactive,
  ref,
  shallowRef,
  watchEffect,
  type Ref,
} from 'vue'

export const name = 'Table'
export const icon = 'table'
export const inputType =
  'Standard.Table.Table.Table | Standard.Table.Column.Column | Standard.Table.Row.Row | Standard.Base.Data.Vector.Vector | Standard.Base.Data.Array.Array | Standard.Base.Data.Map.Map | Any'
export const defaultPreprocessor = [
  'Standard.Visualization.Table.Visualization',
  'prepare_visualization',
  '1000',
] as const

type Data = Error | Matrix | ObjectMatrix | UnknownTable

interface Error {
  type: undefined
  error: string
  all_rows_count?: undefined
}

interface ValueType {
  constructor: string
  display_text: string
}

interface Matrix {
  type: 'Matrix'
  column_count: number
  all_rows_count: number
  json: unknown[][]
  value_type: ValueType[]
}

interface ObjectMatrix {
  type: 'Object_Matrix'
  column_count: number
  all_rows_count: number
  json: object[]
  value_type: ValueType[]
}

interface UnknownTable {
  // This is INCORRECT. It is actually a string, however we do not need to access this.
  // Setting it to `string` breaks the discriminated union detection that is being used to
  // distinguish `Matrix` and `ObjectMatrix`.
  type: undefined
  json: unknown
  all_rows_count?: number
  header: string[] | undefined
  data: unknown[][] | undefined
  value_type: ValueType[]
  has_index_col: boolean | undefined
}

declare module 'ag-grid-enterprise' {
  // These type parameters are defined on the original interface.
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  interface AbstractColDef<TData, TValue> {
    field: string
  }
}

if (typeof import.meta.env.VITE_ENSO_AG_GRID_LICENSE_KEY !== 'string') {
  console.warn('The AG_GRID_LICENSE_KEY is not defined.')
}
</script>

<script setup lang="ts">
const { LicenseManager, Grid } = await import('ag-grid-enterprise')

const props = defineProps<{ data: Data }>()
const emit = defineEmits<{
  'update:preprocessor': [module: string, method: string, ...args: string[]]
}>()
const config = useVisualizationConfig()

const INDEX_FIELD_NAME = '#'
const TABLE_NODE_TYPE = 'Standard.Table.Table.Table'
const VECTOR_NODE_TYPE = 'Standard.Base.Data.Vector.Vector'
const COLUMN_NODE_TYPE = 'Standard.Table.Column.Column'

const rowLimit = ref(0)
const page = ref(0)
const pageLimit = ref(0)
const rowCount = ref(0)
const isTruncated = ref(false)
const tableNode = ref<HTMLElement>()
const dataGroupingMap = shallowRef<Map<string, boolean>>()
useAutoBlur(tableNode)
const widths = reactive(new Map<string, number>())
const defaultColDef = {
  editable: false,
  sortable: true as boolean,
  filter: true,
  resizable: true,
  minWidth: 25,
  headerValueGetter: (params: HeaderValueGetterParams) => params.colDef.field,
  cellRenderer: cellRenderer,
  cellClass: cellClass,
}
const agGridOptions: Ref<GridOptions & Required<Pick<GridOptions, 'defaultColDef'>>> = ref({
  headerHeight: 26,
  rowHeight: 22,
  rowData: [],
  columnDefs: [],
  defaultColDef: defaultColDef as typeof defaultColDef & { manuallySized: boolean },
  onFirstDataRendered: updateColumnWidths,
  onRowDataUpdated: updateColumnWidths,
  onColumnResized: lockColumnSize,
  copyHeadersToClipboard: true,
  sendToClipboard: ({ data }: { data: string }) => sendToClipboard(data),
  suppressFieldDotNotation: true,
  enableRangeSelection: true,
  popupParent: document.body,
})

const isRowCountSelectorVisible = computed(() => rowCount.value >= 1000)
const selectableRowLimits = computed(() => {
  const defaults = [1000, 2500, 5000, 10000, 25000, 50000, 100000].filter(
    (r) => r <= rowCount.value,
  )
  if (rowCount.value < 100000 && !defaults.includes(rowCount.value)) {
    defaults.push(rowCount.value)
  }
  if (!defaults.includes(rowLimit.value)) {
    defaults.push(rowLimit.value)
  }
  return defaults
})

const numberFormatGroupped = new Intl.NumberFormat(undefined, {
  style: 'decimal',
  maximumFractionDigits: 12,
  useGrouping: true,
})

const numberFormat = new Intl.NumberFormat(undefined, {
  style: 'decimal',
  maximumFractionDigits: 12,
  useGrouping: false,
})

function formatNumber(params: ICellRendererParams) {
  const valueType = params.value?.type
  const value = valueType === 'BigInt' ? BigInt(params.value?.value) : params.value
  const needsGrouping = dataGroupingMap.value?.get(params.colDef?.field || '')
  return needsGrouping ? numberFormatGroupped.format(value) : numberFormat.format(value)
}

function setRowLimit(newRowLimit: number) {
  if (newRowLimit !== rowLimit.value) {
    rowLimit.value = newRowLimit
    emit(
      'update:preprocessor',
      'Standard.Visualization.Table.Visualization',
      'prepare_visualization',
      newRowLimit.toString(),
    )
  }
}

function escapeHTML(str: string) {
  const mapping: Record<string, string> = {
    '&': '&amp;',
    '<': '&lt;',
    '"': '&quot;',
    "'": '&#39;',
    '>': '&gt;',
  }
  return str.replace(/[&<>"']/g, (m) => mapping[m]!)
}

function cellClass(params: CellClassParams) {
  if (params.colDef.field === '#') return null
  if (typeof params.value === 'number' || params.value === null) return 'ag-right-aligned-cell'
  if (typeof params.value === 'object') {
    const valueType = params.value?.type
    if (valueType === 'BigInt' || valueType === 'Float') return 'ag-right-aligned-cell'
  }
  return null
}

function cellRenderer(params: ICellRendererParams) {
  // Convert's the value into a display string.
  if (params.value === null) return '<span style="color:grey; font-style: italic;">Nothing</span>'
  else if (params.value === undefined) return ''
  else if (params.value === '') return '<span style="color:grey; font-style: italic;">Empty</span>'
  else if (typeof params.value === 'number') return formatNumber(params)
  else if (Array.isArray(params.value)) return `[Vector ${params.value.length} items]`
  else if (typeof params.value === 'object') {
    const valueType = params.value?.type
    if (valueType === 'BigInt') return formatNumber(params)
    else if (valueType === 'Float')
      return `<span style="color:grey; font-style: italic;">${params.value?.value ?? 'Unknown'}</span>`
    else if ('_display_text_' in params.value && params.value['_display_text_'])
      return String(params.value['_display_text_'])
    else return `{ ${valueType} Object }`
  } else return escapeHTML(params.value.toString())
}

function addRowIndex(data: object[]): object[] {
  return data.map((row, i) => ({ [INDEX_FIELD_NAME]: i, ...row }))
}

function toField(name: string, valueType?: ValueType | null | undefined): ColDef {
  const valType = valueType ? valueType.constructor : null
  const displayValue = valueType ? valueType.display_text : null
  let icon
  switch (valType) {
    case 'Char':
      icon = 'text3'
      break
    case 'Boolean':
      icon = 'check'
      break
    case 'Integer':
    case 'Float':
    case 'Decimal':
    case 'Byte':
      icon = 'math'
      break
    case 'Date':
    case 'Date_Time':
      icon = 'calendar'
      break
    case 'Time':
      icon = 'time'
      break
    case 'Mixed':
      icon = 'mixed'
  }
  const svgTemplate = `<svg viewBox="0 0 16 16" width="16" height="16"> <use xlink:href="${icons}#${icon}"/> </svg>`
  const template =
    icon ?
      `<div style='display:flex; flex-direction:row; justify-content:space-between; width:inherit;'> ${name} <span ref="eMenu" class="ag-header-icon ag-header-cell-menu-button"> </span> ${svgTemplate}</div>`
    : `<div>${name}</div>`
  return {
    field: name,
    headerComponentParams: {
      template,
    },
    headerTooltip: displayValue ? displayValue : '',
  }
}

const getPattern = (index: number) =>
  Pattern.new((ast) =>
    Ast.App.positional(
      Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('at')!),
      Ast.tryNumberToEnso(index, ast.module)!,
    ),
  )

const getTablePattern = (index: number) =>
  Pattern.new((ast) =>
    Ast.OprApp.new(
      ast.module,
      Ast.App.positional(
        Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('rows')!),
        Ast.parse('(..All_Rows)'),
      ),
      '.',
      Ast.App.positional(
        Ast.Ident.new(ast.module, Ast.identifier('get')!),
        Ast.tryNumberToEnso(index, ast.module)!,
      ),
    ),
  )
function createNode(params: CellClickedEvent) {
  if (config.nodeType === VECTOR_NODE_TYPE || config.nodeType === COLUMN_NODE_TYPE) {
    config.createNodes({
      content: getPattern(params.data[INDEX_FIELD_NAME]),
      commit: true,
    })
  }
  if (config.nodeType === TABLE_NODE_TYPE) {
    config.createNodes({
      content: getTablePattern(params.data[INDEX_FIELD_NAME]),
      commit: true,
    })
  }
}

function indexField(): ColDef {
  return { field: INDEX_FIELD_NAME, onCellClicked: (params) => createNode(params) }
}

/** Return a human-readable representation of an object. */
function toRender(content: unknown) {
  return content
}

watchEffect(() => {
  // If the user switches from one visualization type to another, we can receive the raw object.
  const data_ =
    typeof props.data === 'object' ?
      props.data
    : {
        type: typeof props.data,
        json: props.data,
        // eslint-disable-next-line camelcase
        all_rows_count: 1,
        data: undefined,
        // eslint-disable-next-line camelcase
        value_type: undefined,
        // eslint-disable-next-line camelcase
        has_index_col: false,
      }
  const options = agGridOptions.value
  if (options.api == null) {
    return
  }

  let columnDefs: ColDef[] = []
  let rowData: object[] = []

  if ('error' in data_) {
    columnDefs = [
      {
        field: 'Error',
        cellStyle: { 'white-space': 'normal' },
      },
    ]
    rowData = [{ Error: data_.error }]
  } else if (data_.type === 'Matrix') {
    columnDefs.push(indexField())
    for (let i = 0; i < data_.column_count; i++) {
      columnDefs.push(toField(i.toString()))
    }
    rowData = addRowIndex(data_.json)
    isTruncated.value = data_.all_rows_count !== data_.json.length
  } else if (data_.type === 'Object_Matrix') {
    columnDefs.push(indexField())
    let keys = new Set<string>()
    for (const val of data_.json) {
      if (val != null) {
        Object.keys(val).forEach((k) => {
          if (!keys.has(k)) {
            keys.add(k)
            columnDefs.push(toField(k))
          }
        })
      }
    }
    rowData = addRowIndex(data_.json)
    isTruncated.value = data_.all_rows_count !== data_.json.length
  } else if (Array.isArray(data_.json)) {
    columnDefs = [indexField(), toField('Value')]
    rowData = data_.json.map((row, i) => ({ [INDEX_FIELD_NAME]: i, Value: toRender(row) }))
    isTruncated.value = data_.all_rows_count ? data_.all_rows_count !== data_.json.length : false
  } else if (data_.json !== undefined) {
    columnDefs = [toField('Value')]
    rowData = [{ Value: toRender(data_.json) }]
  } else {
    const dataHeader =
      ('header' in data_ ? data_.header : [])?.map((v, i) => {
        const valueType = data_.value_type ? data_.value_type[i] : null
        return toField(v, valueType)
      }) ?? []

    columnDefs = data_.has_index_col ? [indexField(), ...dataHeader] : dataHeader
    const rows = data_.data && data_.data.length > 0 ? data_.data[0]?.length ?? 0 : 0
    rowData = Array.from({ length: rows }, (_, i) => {
      const shift = data_.has_index_col ? 1 : 0
      return Object.fromEntries(
        columnDefs.map((h, j) => {
          return [
            h.field,
            toRender(h.field === INDEX_FIELD_NAME ? i : data_.data?.[j - shift]?.[i]),
          ]
        }),
      )
    })
    isTruncated.value = data_.all_rows_count !== rowData.length
  }

  // Update paging
  const newRowCount = data_.all_rows_count == null ? 1 : data_.all_rows_count
  rowCount.value = newRowCount
  const newPageLimit = Math.ceil(newRowCount / rowLimit.value)
  pageLimit.value = newPageLimit
  if (page.value > newPageLimit) {
    page.value = newPageLimit
  }

  if (rowData.length) {
    const headers = Object.keys(rowData[0] as object)
    const headerGroupingMap = new Map()
    headers.forEach((header) => {
      const needsGrouping = rowData.some((row: any) => {
        if (header in row && row[header] != null) {
          const value = typeof row[header] === 'object' ? row[header].value : row[header]
          return value > 9999
        }
      })
      headerGroupingMap.set(header, needsGrouping)
    })
    dataGroupingMap.value = headerGroupingMap
  }

  // If an existing grid, merge width from manually sized columns.
  const newWidths = new Map<string, number>()
  const mergedColumnDefs = columnDefs.map((columnDef) => {
    if (!columnDef.field) return columnDef
    const width = widths.get(columnDef.field)
    if (width != null) newWidths.set(columnDef.field, (columnDef.width = width))
    return columnDef
  })
  widths.clear()
  for (const [key, value] of newWidths) widths.set(key, value)

  // If data is truncated, we cannot rely on sorting/filtering so will disable.
  options.defaultColDef.filter = !isTruncated.value
  options.defaultColDef.sortable = !isTruncated.value
  options.api.setColumnDefs(mergedColumnDefs)
  options.api.setRowData(rowData)
})

function updateColumnWidths() {
  const columnApi = agGridOptions.value.columnApi
  if (columnApi == null) {
    console.warn('AG Grid column API does not exist.')
    return
  }
  const cols = columnApi.getAllGridColumns().filter((c) => {
    const field = c.getColDef().field
    return field && !widths.has(field)
  })
  columnApi.autoSizeColumns(cols)
}

function lockColumnSize(e: ColumnResizedEvent) {
  // Check if the resize is finished, and it's not from the API (which is triggered by us).
  if (!e.finished || e.source === 'api') return
  // If the user manually resized (or manually autosized) a column, we don't want to auto-size it
  // on a resize.
  const manuallySized = e.source !== 'autosizeColumns'
  for (const column of e.columns ?? []) {
    const field = column.getColDef().field
    if (field && manuallySized) widths.set(field, column.getActualWidth())
  }
}

/** Copy the provided TSV-formatted table data to the clipboard.
 *
 * The data will be copied as `text/plain` TSV data for spreadsheet applications, and an Enso-specific MIME section for
 * pasting as a new table node.
 *
 * By default, AG Grid writes only `text/plain` TSV data to the clipboard. This is sufficient to paste into spreadsheet
 * applications, which are liberal in what they try to interpret as tabular data; however, when pasting into Enso, the
 * application needs to be able to distinguish tabular clipboard contents to choose the correct paste action.
 *
 * Our heuristic to identify clipboard data from applications like Excel and Google Sheets is to check for a <table> tag
 * in the clipboard `text/html` data. If we were to add a `text/html` section to the data so that it could be recognized
 * like other spreadsheets, when pasting into other applications some applications might use the `text/html` data in
 * preference to the `text/plain` content--so we would need to construct an HTML table that fully represents the
 * content.
 *
 * To avoid that complexity, we bypass our table-data detection by including application-specific data in the clipboard
 * content. This data contains a ready-to-paste node that constructs an Enso table from the provided TSV.
 */
function sendToClipboard(tsvData: string) {
  return writeClipboard({
    ...clipboardNodeData([{ expression: tsvTableToEnsoExpression(tsvData) }]),
    'text/plain': tsvData,
  })
}

// ===============
// === Updates ===
// ===============

onMounted(() => {
  setRowLimit(1000)
  const agGridLicenseKey = import.meta.env.VITE_ENSO_AG_GRID_LICENSE_KEY
  if (typeof agGridLicenseKey === 'string') {
    LicenseManager.setLicenseKey(agGridLicenseKey)
  } else if (import.meta.env.DEV) {
    // Hide annoying license validation errors in dev mode when the license is not defined. The
    // missing define warning is still displayed to not forget about it, but it isn't as obnoxious.
    const origValidateLicense = LicenseManager.prototype.validateLicense
    LicenseManager.prototype.validateLicense = function (this) {
      if (!('licenseManager' in this))
        Object.defineProperty(this, 'licenseManager', {
          configurable: true,
          set(value: any) {
            Object.getPrototypeOf(value).validateLicense = () => {}
            delete this.licenseManager
            this.licenseManager = value
          },
        })
      origValidateLicense.call(this)
    }
  }
  // TODO: consider using Vue component instead: https://ag-grid.com/vue-data-grid/getting-started/
  new Grid(tableNode.value!, agGridOptions.value)
  updateColumnWidths()
})

onUnmounted(() => {
  agGridOptions.value.api?.destroy()
})
</script>

<template>
  <VisualizationContainer :belowToolbar="true" :overflow="true">
    <div ref="rootNode" class="TableVisualization" @wheel.stop @pointerdown.stop>
      <div class="table-visualization-status-bar">
        <select
          v-if="isRowCountSelectorVisible"
          @change="setRowLimit(Number(($event.target as HTMLOptionElement).value))"
        >
          <option
            v-for="limit in selectableRowLimits"
            :key="limit"
            :value="limit"
            v-text="limit"
          ></option>
        </select>
        <span
          v-if="isRowCountSelectorVisible && isTruncated"
          v-text="` of ${rowCount} rows (Sorting/Filtering disabled).`"
        ></span>
        <span v-else-if="isRowCountSelectorVisible" v-text="' rows.'"></span>
        <span v-else-if="rowCount === 1" v-text="'1 row.'"></span>
        <span v-else v-text="`${rowCount} rows.`"></span>
      </div>
      <div ref="tableNode" class="scrollable ag-theme-alpine"></div>
    </div>
  </VisualizationContainer>
</template>

<style scoped>
.TableVisualization {
  display: flex;
  flex-flow: column;
  position: relative;
  height: 100%;
}

.ag-theme-alpine {
  --ag-grid-size: 3px;
  --ag-list-item-height: 20px;
  flex-grow: 1;
}

.table-visualization-status-bar {
  height: 20px;
  background-color: white;
  font-size: 14px;
  white-space: nowrap;
  padding: 0 5px;
  overflow: hidden;
}
</style>

<style>
.TableVisualization > .ag-theme-alpine > .ag-root-wrapper.ag-layout-normal {
  border-radius: 0 0 var(--radius-default) var(--radius-default);
}
</style>
