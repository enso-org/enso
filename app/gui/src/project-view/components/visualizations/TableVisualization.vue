<script lang="ts">
import icons from '@/assets/icons.svg'
import AgGridTableView, { commonContextMenuActions } from '@/components/shared/AgGridTableView.vue'
import {
  useTableVizToolbar,
  type SortModel,
} from '@/components/visualizations/TableVisualization/tableVizToolbar'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { LINKABLE_URL_REGEX } from '@/util/link'
import { useVisualizationConfig } from '@/util/visualizationBuiltins'
import type {
  CellClassParams,
  CellDoubleClickedEvent,
  ColDef,
  ICellRendererParams,
  ITooltipParams,
  SortChangedEvent,
} from 'ag-grid-enterprise'
import { computed, onMounted, ref, shallowRef, watchEffect, type Ref } from 'vue'
import { TableVisualisationTooltip } from './TableVisualization/TableVisualisationTooltip'
import { getCellValueType, isNumericType } from './TableVisualization/tableVizUtils'

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
  child_label: string
  visualization_header: string
}

interface Excel_Workbook {
  type: 'Excel_Workbook'
  column_count: number
  all_rows_count: number
  sheet_names: string[]
  json: unknown[][]
  get_child_node_action: string
  child_label: string
  visualization_header: string
}

interface ObjectMatrix {
  type: 'Object_Matrix'
  column_count: number
  all_rows_count: number
  json: object[]
  value_type: ValueType[]
  get_child_node_action: string
  child_label: string
  visualization_header: string
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
  child_label: string
  visualization_header: string
  data_quality_metrics?: DataQualityMetric[]
}

type DataQualityMetric = {
  name: string
  percentage_value: number[]
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
const ROW_NODE_TYPE = 'Standard.Table.Row.Row'

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
  cellStyle: { 'padding-left': 0, 'border-right': '1px solid #C0C0C0' },
  contextMenuItems: [
    commonContextMenuActions.copy,
    commonContextMenuActions.copyWithHeaders,
    'separator',
    'export',
  ],
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
    const replaceLinks = replaceLinksWithTag(htmlEscaped)
    return replaceLinks.replace(/^\s+|\s+$/g, '&nbsp;')
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

  const replaceLinks = replaceLinksWithTag(replaceSpaces)

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

function replaceLinksWithTag(str: string) {
  return str.replace(
    LINKABLE_URL_REGEX,
    (url: string) => `<a href="${url}" target="_blank" class="link">${url}</a>`,
  )
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
    if (isNumericType(valueType)) return 'ag-right-aligned-cell'
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
    if (valueType === 'Float')
      return `<span style="color:grey; font-style: italic;">${params.value?.value ?? 'Unknown'}</span>`
    else if (isNumericType(valueType)) return formatNumber(params)
    else if ('_display_text_' in params.value && params.value['_display_text_'])
      return String(params.value['_display_text_'])
    else return `{ ${valueType} Object }`
  } else return escapeHTML(params.value.toString())
}

function addRowIndex(data: object[]): object[] {
  return data.map((row, i) => ({ [INDEX_FIELD_NAME]: i, ...row }))
}

function getValueTypeIcon(valueType: string) {
  switch (valueType) {
    case 'Char':
      return 'text3'
    case 'Boolean':
      return 'check'
    case 'Integer':
    case 'Float':
    case 'Decimal':
    case 'Byte':
      return 'math'
    case 'Date':
    case 'Date_Time':
      return 'calendar'
    case 'Time':
      return 'time'
    case 'Mixed':
      return 'mixed'
  }
}

/**
 * Generates the column definition for the table vizulization, including displaying the data value type and
 * data quality indicators.
 * @param name - The name which will be displayed in the table header and used to idenfiy the column.
 * @param [options.index] - The index of column the corresponds to the data in the `dataQuality` arrays
 * (`number_of_nothing` and `number_of_whitespace`). This identifies the correct indicators for each column
 * to be displayed in the toolip. If absent the data quality metrics will not be shown.
 * @param [options.valueType] - The data type of the column, displayed as an icon
 * and in text within the tooltip. If absent the value type icon and text will not be shown.
 */
function toField(
  name: string,
  options: { index?: number; valueType?: ValueType | null | undefined } = {},
): ColDef {
  const { index, valueType } = options

  const displayValue = valueType ? valueType.display_text : null
  const icon = valueType ? getValueTypeIcon(valueType.constructor) : null

  const dataQualityMetrics =
    typeof props.data === 'object' && 'data_quality_metrics' in props.data ?
      props.data.data_quality_metrics.map((metric: DataQualityMetric) => {
        return { [metric.name]: metric.percentage_value[index!] ?? 0 }
      })
    : []

  const showDataQuality =
    dataQualityMetrics.filter((obj) => (Object.values(obj)[0] as number) > 0).length > 0

  const getSvgTemplate = (icon: string) =>
    `<svg viewBox="0 0 16 16" width="16" height="16"> <use xlink:href="${icons}#${icon}"/> </svg>`
  const svgTemplateWarning = showDataQuality ? getSvgTemplate('warning') : ''
  const menu = `<span data-ref="eMenu" class="ag-header-icon ag-header-cell-menu-button"> </span>`
  const filterButton = `<span data-ref="eFilterButton" class="ag-header-icon ag-header-cell-filter-button" aria-hidden="true"></span>`
  const sort = `
      <span data-ref="eFilter" class="ag-header-icon ag-header-label-icon ag-filter-icon" aria-hidden="true"></span>
      <span data-ref="eSortOrder" class="ag-header-icon ag-sort-order" aria-hidden="true"></span>
      <span data-ref="eSortAsc" class="ag-header-icon ag-sort-ascending-icon" aria-hidden="true"></span>
      <span data-ref="eSortDesc" class="ag-header-icon ag-sort-descending-icon" aria-hidden="true"></span>
      <span data-ref="eSortNone" class="ag-header-icon ag-sort-none-icon" aria-hidden="true"></span>
    `

  const styles = 'display:flex; flex-direction:row; justify-content:space-between; width:inherit;'
  const template =
    icon ?
      `<span style='${styles}'><span data-ref="eLabel" class="ag-header-cell-label" role="presentation" style='${styles}'><span data-ref="eText" class="ag-header-cell-text"></span></span>${menu} ${filterButton} ${sort} ${getSvgTemplate(icon)} ${svgTemplateWarning}</span>`
    : `<span style='${styles}' data-ref="eLabel"><span data-ref="eText" class="ag-header-cell-label"></span> ${menu} ${filterButton} ${sort} ${svgTemplateWarning}</span>`

  return {
    field: name,
    headerName: name, // AGGrid would demangle it its own way if not specified.
    headerComponentParams: {
      template,
      setAriaSort: () => {},
    },
    tooltipComponent: TableVisualisationTooltip,
    headerTooltip: displayValue ? displayValue : '',
    tooltipComponentParams: {
      dataQualityMetrics,
      total: typeof props.data === 'object' ? props.data.all_rows_count : 0,
      showDataQuality,
    },
  }
}

function toRowField(name: string, index: number, valueType?: ValueType | null | undefined) {
  return {
    ...toField(name, { index, valueType }),
    cellDataType: false,
  }
}

function getAstPattern(selector?: string | number, action?: string) {
  if (action && selector != null) {
    return Pattern.new<Ast.Expression>((ast) =>
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
  params: CellDoubleClickedEvent,
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

interface LinkFieldOptions {
  tooltipValue?: string | undefined
  headerName?: string | undefined
  getChildAction?: string | undefined
  castValueTypes?: string | undefined
}

function toLinkField(fieldName: string, options: LinkFieldOptions = {}): ColDef {
  const { tooltipValue, headerName, getChildAction, castValueTypes } = options
  return {
    headerName: headerName ? headerName : fieldName,
    field: fieldName,
    onCellDoubleClicked: (params) => createNode(params, fieldName, getChildAction, castValueTypes),
    tooltipValueGetter: (params: ITooltipParams) =>
      params.node?.rowPinned === 'top' ?
        null
      : `Double click to view this ${tooltipValue ?? 'value'} in a separate component`,
    cellRenderer: (params: ICellRendererParams) => `<div class='link'> ${params.value} </div>`,
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
        child_label: undefined,
        // eslint-disable-next-line camelcase
        visualization_header: undefined,
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
    columnDefs.value = [
      toLinkField(INDEX_FIELD_NAME, {
        tooltipValue: data_.child_label,
        headerName: data_.visualization_header,
        getChildAction: data_.get_child_node_action,
      }),
    ]
    for (let i = 0; i < data_.column_count; i++) {
      columnDefs.value.push(toField(i.toString()))
    }
    rowData.value = addRowIndex(data_.json)
    isTruncated.value = data_.all_rows_count !== data_.json.length
  } else if (data_.type === 'Object_Matrix') {
    columnDefs.value = [
      toLinkField(INDEX_FIELD_NAME, {
        tooltipValue: data_.child_label,
        headerName: data_.visualization_header,
        getChildAction: data_.get_child_node_action,
      }),
    ]
    const keys = new Set<string>()
    for (const val of data_.json) {
      if (val != null) {
        Object.keys(val).forEach((k, i) => {
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
    columnDefs.value = [
      toLinkField('Value', {
        tooltipValue: data_.child_label,
        headerName: data_.visualization_header,
        getChildAction: data_.get_child_node_action,
      }),
    ]
    rowData.value = data_.sheet_names.map((name) => ({ Value: name }))
  } else if (Array.isArray(data_.json)) {
    columnDefs.value = [
      toLinkField(INDEX_FIELD_NAME, {
        tooltipValue: data_.child_label,
        headerName: data_.visualization_header,
        getChildAction: data_.get_child_node_action,
      }),
      toField('Value'),
    ]
    rowData.value = data_.json.map((row, i) => ({ [INDEX_FIELD_NAME]: i, Value: toRender(row) }))
    isTruncated.value = data_.all_rows_count ? data_.all_rows_count !== data_.json.length : false
  } else if (data_.json !== undefined) {
    columnDefs.value =
      data_.links ?
        [
          toLinkField('Value', {
            tooltipValue: data_.child_label,
            headerName: data_.visualization_header,
            getChildAction: data_.get_child_node_action,
          }),
        ]
      : [toField('Value')]
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
          return toLinkField(v, {
            tooltipValue: data_.child_label,
            headerName: data_.visualization_header,
            getChildAction: data_.get_child_node_action,
            castValueTypes: data_.link_value_type,
          })
        }
        if (config.nodeType === ROW_NODE_TYPE) {
          return toRowField(v, i, valueType)
        }
        return toField(v, { index: i, valueType })
      }) ?? []

    columnDefs.value =
      data_.has_index_col ?
        [
          toLinkField(INDEX_FIELD_NAME, {
            tooltipValue: data_.child_label,
            headerName: data_.visualization_header,
            getChildAction: data_.get_child_node_action,
          }),
          ...dataHeader,
        ]
      : dataHeader
    const rows = data_.data && data_.data.length > 0 ? (data_.data[0]?.length ?? 0) : 0
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
          return value > 999999 || value < -999999
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

const colTypeMap = computed(() => {
  const colMap: Map<string, string> = new Map()
  if (typeof props.data === 'object' && !('error' in props.data)) {
    const valueTypes = 'value_type' in props.data ? props.data.value_type : []
    const headers = 'header' in props.data ? props.data.header : []
    headers?.forEach((header, index) => {
      if (valueTypes[index]) {
        colMap.set(header, valueTypes[index].constructor)
      }
    })
  }
  return colMap
})

const getColumnValueToEnso = (columnName: string) => {
  const columnType = colTypeMap.value.get(columnName) ?? ''
  if (isNumericType(columnType)) {
    return (item: string, module: Ast.MutableModule) => Ast.tryNumberToEnso(Number(item), module)!
  }
  if (columnType === 'Date') {
    return (item: string, module: Ast.MutableModule) => createDateValue(item, module)
  }
  if (columnType === 'Time') {
    return (item: string, module: Ast.MutableModule) =>
      createDateTimeValue('Time_Of_Day.parse (__)', item, module)
  }
  if (columnType === 'Date_Time') {
    return (item: string, module: Ast.MutableModule) =>
      createDateTimeValue('Date_Time.parse (__)', item, module)
  }
  if (columnType == 'Mixed') {
    return (item: string, module: Ast.MutableModule) => {
      const parsedCellType = getCellValueType(item)
      return getFormattedValueForCell(item, module, parsedCellType)
    }
  }
  return (item: string) => Ast.TextLiteral.new(item)
}

const getFormattedValueForCell = (item: string, module: Ast.MutableModule, cellType: string) => {
  if (isNumericType(cellType)) {
    return Ast.tryNumberToEnso(Number(item), module)!
  }
  if (cellType === 'Date') {
    return createDateValue(item, module)
  }
  if (cellType === 'Time') {
    return createDateTimeValue('Time_Of_Day.parse (__)', item, module)
  }
  if (cellType === 'Date_Time') {
    return createDateTimeValue('Date_Time.parse (__)', item, module)
  }
  return Ast.TextLiteral.new(item)
}

const createDateTimeValue = (patternString: string, item: string, module: Ast.MutableModule) => {
  const pattern = Pattern.parseExpression(patternString)!
  return pattern.instantiateCopied([Ast.TextLiteral.new(item, module)])
}

const createDateValue = (item: string, module: Ast.MutableModule) => {
  const dateOrTimePattern = Pattern.parseExpression('(Date.new __ __ __)')
  const dateTimeParts = item
    .match(/\d+/g)!
    .map((part) => Ast.tryNumberToEnso(Number(part), module)!)
  return dateOrTimePattern.instantiateCopied([...dateTimeParts])
}

function checkSortAndFilter(e: SortChangedEvent) {
  const gridApi = e.api
  if (gridApi == null) {
    console.warn('AG Grid column API does not exist.')
    isCreateNodeEnabled.value = false
    return
  }
  const colState = gridApi.getColumnState()
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
    getColumnValueToEnso,
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
  font-size: 14px;
  color: var(--color-ag-header-text);
  white-space: nowrap;
  padding: 0 5px;
  overflow: hidden;
}

.TableVisualization:deep(.ag-root-wrapper) {
  --ag-wrapper-border-radius: 0 0 var(--radius-default) var(--radius-default);
  border: none;
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

.ag-header-cell .myclass {
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  width: inherit;
}
</style>
