<script lang="ts">
import icons from '@/assets/icons.svg'
import AgGridTableView, { commonContextMenuActions } from '@/components/shared/AgGridTableView.vue'
import { SortModel, useTableVizToolbar } from '@/components/visualizations/tableVizToolbar'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { useVisualizationConfig } from '@/util/visualizationBuiltins'
import type {
  CellClassParams,
  CellClickedEvent,
  ColDef,
  ICellRendererParams,
  SortChangedEvent,
} from 'ag-grid-enterprise'
import { computed, onMounted, ref, shallowRef, watchEffect, type Ref } from 'vue'

export const name = 'Table'
export const icon = 'table'
export const inputType =
  'Standard.Table.Table.Table | Standard.Table.Column.Column | Standard.Table.Row.Row | Standard.Base.Data.Vector.Vector | Standard.Base.Data.Array.Array | Standard.Base.Data.Map.Map | Any'
export const defaultPreprocessor = [
  'Standard.Visualization.Table.Visualization',
  'prepare_visualization',
  '1000',
] as const

type Data = number | string | Error | Matrix | ObjectMatrix | UnknownTable | Excel_Workbook

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
  get_child_node_action: string
}

interface Excel_Workbook {
  type: 'Excel_Workbook'
  column_count: number
  all_rows_count: number
  sheet_names: string[]
  json: unknown[][]
  get_child_node_action: string
}

interface ObjectMatrix {
  type: 'Object_Matrix'
  column_count: number
  all_rows_count: number
  json: object[]
  value_type: ValueType[]
  get_child_node_action: string
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
  links: string[] | undefined
  get_child_node_action: string
  get_child_node_link_name: string
  link_value_type: string
}

export type TextFormatOptions = 'full' | 'partial' | 'off'
</script>

<script setup lang="ts">
const props = defineProps<{ data: Data }>()
const config = useVisualizationConfig()

const INDEX_FIELD_NAME = '#'
const TABLE_NODE_TYPE = 'Standard.Table.Table.Table'
const DB_TABLE_NODE_TYPE = 'Standard.Database.DB_Table.DB_Table'
const VECTOR_NODE_TYPE = 'Standard.Base.Data.Vector.Vector'
const COLUMN_NODE_TYPE = 'Standard.Table.Column.Column'
const EXCEL_WORKBOOK_NODE_TYPE = 'Standard.Table.Excel.Excel_Workbook.Excel_Workbook'
const ROW_NODE_TYPE = 'Standard.Table.Row.Row'
const SQLITE_CONNECTIONS_NODE_TYPE =
  'Standard.Database.Internal.SQLite.SQLite_Connection.SQLite_Connection'
const POSTGRES_CONNECTIONS_NODE_TYPE =
  'Standard.Database.Internal.Postgres.Postgres_Connection.Postgres_Connection'
const SNOWFLAKE_CONNECTIONS_NODE_TYPE =
  'Standard.Snowflake.Snowflake_Connection.Snowflake_Connection'

const rowLimit = ref(0)
const page = ref(0)
const pageLimit = ref(0)
const rowCount = ref(0)
const showRowCount = ref(true)
const isTruncated = ref(false)
const isCreateNodeEnabled = ref(false)
const filterModel = ref({})
const sortModel = ref<SortModel[]>([])
const dataGroupingMap = shallowRef<Map<string, boolean>>()
const defaultColDef: Ref<ColDef> = ref({
  editable: false,
  sortable: true,
  filter: true,
  resizable: true,
  minWidth: 25,
  cellRenderer: cellRenderer,
  cellClass: cellClass,
  contextMenuItems: [commonContextMenuActions.copy, 'copyWithHeaders', 'separator', 'export'],
} satisfies ColDef)
const rowData = ref<Record<string, any>[]>([])
const columnDefs: Ref<ColDef[]> = ref([])

const textFormatterSelected = ref<TextFormatOptions>('partial')

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

const newNodeSelectorValues = computed(() => {
  let tooltipValue
  let headerName
  switch (config.nodeType) {
    case COLUMN_NODE_TYPE:
    case VECTOR_NODE_TYPE:
    case ROW_NODE_TYPE:
      tooltipValue = 'value'
      break
    case EXCEL_WORKBOOK_NODE_TYPE:
      tooltipValue = 'sheet'
      headerName = 'Sheets'
      break
    case SQLITE_CONNECTIONS_NODE_TYPE:
    case POSTGRES_CONNECTIONS_NODE_TYPE:
    case SNOWFLAKE_CONNECTIONS_NODE_TYPE:
      tooltipValue = 'table'
      headerName = 'Tables'
      break
    case TABLE_NODE_TYPE:
    case DB_TABLE_NODE_TYPE:
      tooltipValue = 'row'
  }
  return {
    tooltipValue,
    headerName,
  }
})

const isFilterSortNodeEnabled = computed(
  () => config.nodeType === TABLE_NODE_TYPE || config.nodeType === DB_TABLE_NODE_TYPE,
)

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
  let value
  if (valueType === 'Integer') {
    value = BigInt(params.value?.value)
  } else if (valueType === 'Decimal') {
    value = Number(params.value?.value)
  } else {
    value = params.value
  }
  const needsGrouping = dataGroupingMap.value?.get(params.colDef?.field || '')
  return needsGrouping ? numberFormatGroupped.format(value) : numberFormat.format(value)
}

function formatText(params: ICellRendererParams) {
  const htmlEscaped = params.value
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')

  if (textFormatterSelected.value === 'off') {
    return htmlEscaped.replace(/^\s+|\s+$/g, '&nbsp;')
  }

  const partialMappings = {
    '\r': '<span style="color: #df8800">␍</span> <br>',
    '\n': '<span style="color: #df8800;">␊</span> <br>',
    '\t': '<span style="color: #df8800; white-space: break-spaces;">&#8594;  |</span>',
  }
  const fullMappings = {
    '\r': '<span style="color: #df8800">␍</span> <br>',
    '\n': '<span style="color: #df8800">␊</span> <br>',
    '\t': '<span style="color: #df8800; white-space: break-spaces;">&#8594;  |</span>',
  }

  const replaceSpaces =
    textFormatterSelected.value === 'full' ?
      htmlEscaped.replaceAll(' ', '<span style="color: #df8800">&#183;</span>')
    : htmlEscaped.replace(/ \s+|^ +| +$/g, function (match: string) {
        return `<span style="color: #df8800">${match.replaceAll(' ', '&#183;')}</span>`
      })

  const replaceLinks = replaceSpaces.replace(
    /https?:\/\/([-()_.!~*';/?:@&=+$,A-Za-z0-9])+/g,
    (url: string) => `<a href="${url}" target="_blank" class="link">${url}</a>`,
  )

  const replaceReturns = replaceLinks.replace(
    /\r\n/g,
    '<span style="color: #df8800">␍␊</span> <br>',
  )

  const renderOtherWhitespace = (match: string) => {
    return textFormatterSelected.value === 'full' && match != ' ' ?
        '<span style="color: #df8800">&#9744;</span>'
      : match
  }
  const newString = replaceReturns.replace(/[\s]/g, function (match: string) {
    const mapping = textFormatterSelected.value === 'full' ? fullMappings : partialMappings
    return mapping[match as keyof typeof mapping] || renderOtherWhitespace(match)
  })
  return `<span > ${newString} <span>`
}

function setRowLimit(newRowLimit: number) {
  if (newRowLimit !== rowLimit.value) {
    rowLimit.value = newRowLimit
    config.setPreprocessor(
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
    if (valueType === 'BigInt' || valueType === 'Float' || valueType === 'Decimal')
      return 'ag-right-aligned-cell'
  }
  return null
}

function cellRenderer(params: ICellRendererParams) {
  // Convert's the value into a display string.
  if (params.value === null) return '<span style="color:grey; font-style: italic;">Nothing</span>'
  else if (params.value === undefined) return ''
  else if (params.value === '') return '<span style="color:grey; font-style: italic;">Empty</span>'
  else if (typeof params.value === 'number') return formatNumber(params)
  else if (typeof params.value === 'string') return formatText(params)
  else if (Array.isArray(params.value)) return `[Vector ${params.value.length} items]`
  else if (typeof params.value === 'object') {
    const valueType = params.value?.type
    if (valueType === 'BigInt') return formatNumber(params)
    else if (valueType === 'Decimal') return formatNumber(params)
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
  const menu = `<span ref="eMenu" class="ag-header-icon ag-header-cell-menu-button"> </span>`
  const sort = `
      <span ref="eFilter" class="ag-header-icon ag-header-label-icon ag-filter-icon" aria-hidden="true"></span>
      <span ref="eSortOrder" class="ag-header-icon ag-sort-order" aria-hidden="true"></span>
      <span ref="eSortAsc" class="ag-header-icon ag-sort-ascending-icon" aria-hidden="true"></span>
      <span ref="eSortDesc" class="ag-header-icon ag-sort-descending-icon" aria-hidden="true"></span>
      <span ref="eSortNone" class="ag-header-icon ag-sort-none-icon" aria-hidden="true"></span>
    `
  const template =
    icon ?
      `<span style='display:flex; flex-direction:row; justify-content:space-between; width:inherit;'><span ref="eLabel" class="ag-header-cell-label" role="presentation" style='display:flex; flex-direction:row; justify-content:space-between; width:inherit;'> ${name} </span>  ${menu} ${sort} ${svgTemplate}</span>`
    : `<span ref="eLabel" style='display:flex; flex-direction:row; justify-content:space-between; width:inherit;'>${name} ${menu} ${sort}</span>`
  return {
    field: name,
    headerComponentParams: {
      template,
      setAriaSort: () => {},
    },
    headerTooltip: displayValue ? displayValue : '',
  }
}

function toRowField(name: string, valueType?: ValueType | null | undefined) {
  return {
    ...toField(name, valueType),
    cellDataType: false,
  }
}

function getAstPattern(selector?: string | number, action?: string) {
  if (action && selector != null) {
    return Pattern.new((ast) =>
      Ast.App.positional(
        Ast.PropertyAccess.new(ast.module, ast, Ast.identifier(action)!),
        typeof selector === 'number' ?
          Ast.tryNumberToEnso(selector, ast.module)!
        : Ast.TextLiteral.new(selector, ast.module),
      ),
    )
  }
}

function createNode(
  params: CellClickedEvent,
  selector: string,
  action?: string,
  castValueTypes?: string,
) {
  const selectorKey = params.data[selector]
  const castSelector =
    castValueTypes === 'number' && !isNaN(Number(selectorKey)) ? Number(selectorKey) : selectorKey
  const identifierAction =
    config.nodeType === (COLUMN_NODE_TYPE || VECTOR_NODE_TYPE) ? 'at' : action
  const pattern = getAstPattern(castSelector, identifierAction)
  if (pattern) {
    config.createNodes({
      content: pattern,
      commit: true,
    })
  }
}

function toLinkField(fieldName: string, getChildAction?: string, castValueTypes?: string): ColDef {
  return {
    headerName:
      newNodeSelectorValues.value.headerName ? newNodeSelectorValues.value.headerName : fieldName,
    field: fieldName,
    onCellDoubleClicked: (params) => createNode(params, fieldName, getChildAction, castValueTypes),
    tooltipValueGetter: () => {
      return `Double click to view this ${newNodeSelectorValues.value.tooltipValue} in a separate component`
    },
    cellRenderer: (params: any) => `<div class='link'> ${params.value} </div>`,
  }
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
        links: undefined,
        // eslint-disable-next-line camelcase
        get_child_node_action: undefined,
        // eslint-disable-next-line camelcase
        get_child_node_link_name: undefined,
        // eslint-disable-next-line camelcase
        link_value_type: undefined,
      }
  if ('error' in data_) {
    columnDefs.value = [
      {
        field: 'Error',
        cellStyle: { 'white-space': 'normal' },
      },
    ]
    rowData.value = [{ Error: data_.error }]
  } else if (data_.type === 'Matrix') {
    columnDefs.value = [toLinkField(INDEX_FIELD_NAME, data_.get_child_node_action)]
    for (let i = 0; i < data_.column_count; i++) {
      columnDefs.value.push(toField(i.toString()))
    }
    rowData.value = addRowIndex(data_.json)
    isTruncated.value = data_.all_rows_count !== data_.json.length
  } else if (data_.type === 'Object_Matrix') {
    columnDefs.value = [toLinkField(INDEX_FIELD_NAME, data_.get_child_node_action)]
    const keys = new Set<string>()
    for (const val of data_.json) {
      if (val != null) {
        Object.keys(val).forEach((k) => {
          if (!keys.has(k)) {
            keys.add(k)
            columnDefs.value.push(toField(k))
          }
        })
      }
    }
    rowData.value = addRowIndex(data_.json)
    isTruncated.value = data_.all_rows_count !== data_.json.length
  } else if (data_.type === 'Excel_Workbook') {
    columnDefs.value = [toLinkField('Value', data_.get_child_node_action)]
    rowData.value = data_.sheet_names.map((name) => ({ Value: name }))
  } else if (Array.isArray(data_.json)) {
    columnDefs.value = [
      toLinkField(INDEX_FIELD_NAME, data_.get_child_node_action),
      toField('Value'),
    ]
    rowData.value = data_.json.map((row, i) => ({ [INDEX_FIELD_NAME]: i, Value: toRender(row) }))
    isTruncated.value = data_.all_rows_count ? data_.all_rows_count !== data_.json.length : false
  } else if (data_.json !== undefined) {
    columnDefs.value =
      data_.links ? [toLinkField('Value', data_.get_child_node_action)] : [toField('Value')]
    rowData.value =
      data_.links ?
        data_.links.map((link) => ({
          Value: link,
        }))
      : [{ Value: toRender(data_.json) }]
  } else {
    const dataHeader =
      ('header' in data_ ? data_.header : [])?.map((v, i) => {
        const valueType = data_.value_type ? data_.value_type[i] : null
        if (data_.get_child_node_link_name === v) {
          return toLinkField(v, data_.get_child_node_action, data_.link_value_type)
        }
        if (config.nodeType === ROW_NODE_TYPE) {
          return toRowField(v, valueType)
        }
        return toField(v, valueType)
      }) ?? []

    columnDefs.value =
      data_.has_index_col ?
        [toLinkField(INDEX_FIELD_NAME, data_.get_child_node_action), ...dataHeader]
      : dataHeader
    const rows = data_.data && data_.data.length > 0 ? data_.data[0]?.length ?? 0 : 0
    rowData.value = Array.from({ length: rows }, (_, i) => {
      const shift = data_.has_index_col ? 1 : 0
      return Object.fromEntries(
        columnDefs.value.map((h, j) => {
          return [
            h.field,
            toRender(h.field === INDEX_FIELD_NAME ? i : data_.data?.[j - shift]?.[i]),
          ]
        }),
      )
    })
    isTruncated.value = data_.all_rows_count !== rowData.value.length
  }

  // Update paging
  const newRowCount = data_.all_rows_count == null ? 1 : data_.all_rows_count
  showRowCount.value = !(data_.all_rows_count == null)
  rowCount.value = newRowCount
  const newPageLimit = Math.ceil(newRowCount / rowLimit.value)
  pageLimit.value = newPageLimit
  if (page.value > newPageLimit) {
    page.value = newPageLimit
  }

  if (rowData.value[0]) {
    const headers = Object.keys(rowData.value[0])
    const headerGroupingMap = new Map()
    headers.forEach((header) => {
      const needsGrouping = rowData.value.some((row) => {
        if (header in row && row[header] != null) {
          const value = typeof row[header] === 'object' ? row[header].value : row[header]
          return value > 9999
        }
      })
      headerGroupingMap.set(header, needsGrouping)
    })
    dataGroupingMap.value = headerGroupingMap
  }

  // If data is truncated, we cannot rely on sorting/filtering so will disable.
  defaultColDef.value.filter = !isTruncated.value
  defaultColDef.value.sortable = !isTruncated.value
})

function checkSortAndFilter(e: SortChangedEvent) {
  const gridApi = e.api
  const columnApi = e.columnApi
  if (gridApi == null || columnApi == null) {
    console.warn('AG Grid column API does not exist.')
    isCreateNodeEnabled.value = false
    return
  }
  const colState = columnApi.getColumnState()
  const filter = gridApi.getFilterModel()
  const sort = colState
    .map((cs) => {
      if (cs.sort) {
        return {
          columnName: cs.colId,
          sortDirection: cs.sort,
          sortIndex: cs.sortIndex,
        } as SortModel
      }
    })
    .filter((sort) => sort)
  if (sort.length || Object.keys(filter).length) {
    isCreateNodeEnabled.value = true
    sortModel.value = sort as SortModel[]
    filterModel.value = filter
  } else {
    isCreateNodeEnabled.value = false
    sortModel.value = []
    filterModel.value = {}
  }
}

// ===============
// === Updates ===
// ===============

onMounted(() => {
  setRowLimit(1000)
})

// ===============
// === Toolbar ===
// ===============

config.setToolbar(
  useTableVizToolbar({
    textFormatterSelected,
    filterModel,
    sortModel,
    isDisabled: () => !isCreateNodeEnabled.value,
    isFilterSortNodeEnabled,
    createNodes: config.createNodes,
  }),
)
</script>

<template>
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
      <template v-if="showRowCount">
        <span
          v-if="isRowCountSelectorVisible && isTruncated"
          v-text="` of ${rowCount} rows (Sorting/Filtering disabled).`"
        ></span>
        <span v-else-if="isRowCountSelectorVisible" v-text="' rows.'"></span>
        <span v-else-if="rowCount === 1" v-text="'1 row.'"></span>
        <span v-else v-text="`${rowCount} rows.`"></span>
      </template>
    </div>
    <!-- TODO[ao]: Suspence in theory is not needed here (the entire visualization is inside
     suspense), but for some reason it causes reactivity loop - see https://github.com/enso-org/enso/issues/10782 -->
    <Suspense>
      <AgGridTableView
        class="scrollable grid"
        :columnDefs="columnDefs"
        :rowData="rowData"
        :defaultColDef="defaultColDef"
        :textFormatOption="textFormatterSelected"
        @sortOrFilterUpdated="(e) => checkSortAndFilter(e)"
      />
    </Suspense>
  </div>
</template>

<style scoped>
.TableVisualization {
  display: flex;
  flex-flow: column;
  position: relative;
  height: 100%;
}

.grid {
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

.TableVisualization > .ag-theme-alpine > :deep(.ag-root-wrapper.ag-layout-normal) {
  border-radius: 0 0 var(--radius-default) var(--radius-default);
}

/* Tag selectors are inefficient to compute, and should be replaced with a class selector
 * if possible.
 * See https://vuejs.org/api/sfc-css-features.html#scoped-style-tips */
:deep(.link) {
  color: blue;
  text-decoration: underline;
}

:deep(.link):hover {
  color: darkblue;
}

.button-wrappers {
  display: flex;
  flex-direction: row;
}
</style>
