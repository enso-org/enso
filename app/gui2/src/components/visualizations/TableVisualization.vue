<script lang="ts">
export const name = 'Table'
export const icon = 'table'
export const inputType =
  'Standard.Table.Data.Table.Table | Standard.Table.Data.Column.Column | Standard.Table.Data.Row.Row |Standard.Base.Data.Vector.Vector | Standard.Base.Data.Array.Array | Standard.Base.Data.Map.Map | Any'
export const defaultPreprocessor = [
  'Standard.Visualization.Table.Visualization',
  'prepare_visualization',
  '1000',
] as const

type Data = Error | Matrix | ObjectMatrix | LegacyMatrix | LegacyObjectMatrix | UnknownTable

interface Error {
  type: undefined
  error: string
  all_rows_count?: undefined
}

interface Matrix {
  type: 'Matrix'
  column_count: number
  all_rows_count: number
  json: unknown[][]
}

interface ObjectMatrix {
  type: 'Object_Matrix'
  column_count: number
  all_rows_count: number
  json: object[]
}

interface LegacyMatrix {
  type: undefined
  column_count: number
  all_rows_count: number
  json: unknown[][]
}

interface LegacyObjectMatrix {
  type: undefined
  column_count: number
  all_rows_count: number
  json: object[]
}

interface UnknownTable {
  // This is INCORRECT. It is actually a string, however we do not need to access this.
  // Setting it to `string` breaks the discriminated union detection that is being used to
  // distinguish `Matrix` and `ObjectMatrix`.
  type: undefined
  json: unknown
  all_rows_count?: number
  header: string[] | undefined
  indices_header?: string[]
  data: unknown[][] | undefined
  indices: unknown[][] | undefined
}
</script>

<script setup lang="ts">
import { VisualizationContainer } from '@/util/visualizationBuiltins'
import type {
  ColDef,
  ColumnResizedEvent,
  GridOptions,
  HeaderValueGetterParams,
} from '@ag-grid-community/core'
import '@ag-grid-community/styles/ag-grid.css'
import '@ag-grid-community/styles/ag-theme-alpine.css'
import { computed, onMounted, reactive, ref, watchEffect, type Ref } from 'vue'
const [
  { ClientSideRowModelModule },
  { RangeSelectionModule },
  { Grid, ModuleRegistry },
  { LicenseManager },
] = await Promise.all([
  import('@ag-grid-community/client-side-row-model'),
  import('@ag-grid-enterprise/range-selection'),
  import('@ag-grid-community/core'),
  import('@ag-grid-enterprise/core'),
])

ModuleRegistry.registerModules([ClientSideRowModelModule, RangeSelectionModule])

const props = defineProps<{ data: Data }>()
const emit = defineEmits<{
  'update:preprocessor': [module: string, method: string, ...args: string[]]
}>()

const INDEX_FIELD_NAME = '#'

const rowLimit = ref(0)
const page = ref(0)
const pageLimit = ref(0)
const rowCount = ref(0)
const isTruncated = ref(false)
const tableNode = ref<HTMLElement>()
const widths = reactive(new Map<string, number>())
const defaultColDef = {
  editable: false,
  sortable: true as boolean,
  filter: true,
  resizable: true,
  minWidth: 25,
  headerValueGetter: (params: HeaderValueGetterParams) => params.colDef.field,
  cellRenderer: cellRenderer,
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
  suppressFieldDotNotation: true,
  enableRangeSelection: true,
})

const isRowCountSelectorVisible = computed(() => rowCount.value >= 1000)
const selectableRowLimits = computed(() =>
  [1000, 2500, 5000, 10000, 25000, 50000, 100000].filter((r) => r <= rowCount.value),
)
const wasAutomaticallyAutosized = ref(false)

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

function cellRenderer(params: { value: string | null }) {
  if (params.value === null) return '<span style="color:grey; font-style: italic;">Nothing</span>'
  else if (params.value === undefined) return ''
  else if (params.value === '') return '<span style="color:grey; font-style: italic;">Empty</span>'
  else return escapeHTML(params.value.toString())
}

function addRowIndex(data: object[]): object[] {
  return data.map((row, i) => ({ [INDEX_FIELD_NAME]: i, ...row }))
}

function hasExactlyKeys(keys: string[], obj: object) {
  return (
    Object.keys(obj).length === keys.length &&
    keys.every((k) => Object.prototype.hasOwnProperty.call(obj, k))
  )
}

function isObjectMatrix(data: object): data is LegacyObjectMatrix {
  if (!('json' in data)) {
    return false
  }
  const json = data.json
  const isList = Array.isArray(json) && json[0] != null
  if (!isList || !(typeof json[0] === 'object')) {
    return false
  }
  const firstKeys = Object.keys(json[0])
  return json.every((obj) => hasExactlyKeys(firstKeys, obj))
}

function isMatrix(data: object): data is LegacyMatrix {
  if (!('json' in data)) {
    return false
  }
  const json = data.json
  const isList = Array.isArray(json) && json[0] != null
  if (!isList) {
    return false
  }
  const firstIsArray = Array.isArray(json[0])
  if (!firstIsArray) {
    return false
  }
  const firstLen = json[0].length
  return json.every((d) => d.length === firstLen)
}

function toField(name: string): ColDef {
  return { field: name }
}

function indexField(): ColDef {
  return toField(INDEX_FIELD_NAME)
}

/** Return a human-readable representation of an object. */
function toRender(content: unknown) {
  if (Array.isArray(content)) {
    if (isMatrix({ json: content })) {
      return `[Vector ${content.length} rows x ${content[0].length} cols]`
    } else if (isObjectMatrix({ json: content })) {
      return `[Table ${content.length} rows x ${Object.keys(content[0]).length} cols]`
    } else {
      return `[Vector ${content.length} items]`
    }
  }

  if (typeof content === 'object' && content != null) {
    const type = 'type' in content ? content.type : undefined
    if ('_display_text_' in content && content['_display_text_']) {
      return String(content['_display_text_'])
    } else {
      return `{ ${type} Object }`
    }
  }

  return content
}

watchEffect(() => {
  const data_ = props.data
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
  } else if (isMatrix(data_)) {
    // Kept to allow visualization from older versions of the backend.
    columnDefs = [indexField(), ...data_.json[0]!.map((_, i) => toField(i.toString()))]
    rowData = addRowIndex(data_.json)
    isTruncated.value = data_.all_rows_count !== data_.json.length
  } else if (isObjectMatrix(data_)) {
    // Kept to allow visualization from older versions of the backend.
    columnDefs = [INDEX_FIELD_NAME, ...Object.keys(data_.json[0]!)].map(toField)
    rowData = addRowIndex(data_.json)
    isTruncated.value = data_.all_rows_count !== data_.json.length
  } else if (Array.isArray(data_.json)) {
    columnDefs = [indexField(), toField('Value')]
    rowData = data_.json.map((row, i) => ({ [INDEX_FIELD_NAME]: i, Value: toRender(row) }))
    isTruncated.value = data_.all_rows_count !== data_.json.length
  } else if (data_.json !== undefined) {
    columnDefs = [toField('Value')]
    rowData = [{ Value: toRender(data_.json) }]
  } else {
    const indicesHeader = ('indices_header' in data_ ? data_.indices_header : []).map(toField)
    const dataHeader = ('header' in data_ ? data_.header : [])?.map(toField) ?? []
    columnDefs = [...indicesHeader, ...dataHeader]
    const rows =
      data_.data && data_.data.length > 0
        ? data_.data[0]?.length ?? 0
        : data_.indices && data_.indices.length > 0
        ? data_.indices[0]?.length ?? 0
        : 0
    rowData = Array.from({ length: rows }, (_, i) => {
      const shift = data_.indices ? data_.indices.length : 0
      return Object.fromEntries(
        columnDefs.map((h, j) => [
          h.field,
          toRender(j < shift ? data_.indices?.[j]?.[i] : data_.data?.[j - shift]?.[i]),
        ]),
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
  const manuallySized = e.source !== 'autosizeColumns' || !wasAutomaticallyAutosized.value
  wasAutomaticallyAutosized.value = false
  for (const column of e.columns ?? []) {
    const field = column.getColDef().field
    if (field && manuallySized) widths.set(field, column.getActualWidth())
  }
}

// ===============
// === Updates ===
// ===============

onMounted(() => {
  setRowLimit(1000)
  if ('AG_GRID_LICENSE_KEY' in window && typeof window.AG_GRID_LICENSE_KEY === 'string') {
    LicenseManager.setLicenseKey(window.AG_GRID_LICENSE_KEY)
  } else {
    console.warn('The AG_GRID_LICENSE_KEY is not defined.')
  }
  new Grid(tableNode.value!, agGridOptions.value)
  updateColumnWidths()
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
