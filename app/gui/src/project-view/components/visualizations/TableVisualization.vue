<script lang="ts">
import AgGridTableView, { commonContextMenuActions } from '@/components/shared/AgGridTableView.vue'
import {
  useTableVizToolbar,
  type SortModel,
} from '@/components/visualizations/TableVisualization/tableVizToolbar'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { svgUseHref } from '@/util/icons'
import { ProjectPath } from '@/util/projectPath'
import { useVisualizationConfig } from '@/util/visualizationBuiltins'
import type {
  CellClassParams,
  CellDoubleClickedEvent,
  ColDef,
  ColumnMovedEvent,
  ColumnVisibleEvent,
  GetContextMenuItems,
  GetContextMenuItemsParams,
  ICellRendererParams,
  IServerSideDatasource,
  IServerSideGetRowsRequest,
  ITooltipParams,
  MenuItemDef,
  SetFilterValuesFuncParams,
  SortChangedEvent,
} from 'ag-grid-enterprise'
import {
  computed,
  onMounted,
  ref,
  shallowRef,
  watch,
  watchEffect,
  type ComponentInstance,
  type Ref,
} from 'vue'
import type { ComponentExposed } from 'vue-component-type-helpers'
import { TableVisualisationTooltip } from './TableVisualization/TableVisualisationTooltip'
import {
  isError,
  isGenericGrid,
  type Error,
  type GenericGrid,
} from './TableVisualization/TableVisualisationTypes'
import {
  convertFilterModel,
  convertSortModel,
  createDistinctExpressionTemplate,
  createExpressionRowTemplate,
  type ValueTypeArgumentChild,
  type ValueTypes,
} from './TableVisualization/TableVizDataSourceUtils'
import {
  getCellDataType,
  getFilterParams,
  getFilterType,
} from './TableVisualization/tableVizFilterSetUpUtils'
import { makeFilterModelList, type GridFilterModel } from './TableVisualization/tableVizFilterUtils'
import { TableVizStatusBar } from './TableVisualization/TableVizStatusBar'
import {
  formatText,
  getCellValueType,
  isNumericType,
  type ValueType,
} from './TableVisualization/tableVizUtils'

export const name = 'Table'
export const icon = 'table'
export const inputType =
  'Standard.Table.Table.Table | Standard.Table.Column.Column | Standard.Table.Row.Row | Standard.Base.Data.Vector.Vector | Standard.Base.Data.Array.Array | Standard.Base.Data.Map.Map | Standard.Base.Any.Any'
export const defaultPreprocessor = [
  'Standard.Visualization.Table.Visualization',
  'prepare_visualization',
  '1000',
] as const

type Data = number | string | Error | Matrix | ObjectMatrix | EnsoTableOrColumn | GenericGrid

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

interface EnsoTableOrColumn {
  type: 'EnsoTableOrColumn'
  json: unknown
  all_rows_count?: number
  header: string[] | undefined
  data: unknown[][] | undefined
  value_type: ValueType[]
  has_index_col: boolean | undefined
  links: string[] | undefined
  get_child_node_action: string
  get_child_node_link_name: string
  child_label: string
  visualization_header: string
  data_quality_metrics?: DataQualityMetric[]
  is_using_server_sort_and_filter: boolean
  use_bottom_status_bar: boolean
  enable_create_node: boolean
  requires_number_format: boolean[]
  is_using_multi_filter: boolean[]
  table_version_hash?: string
}

type DataQualityMetricNumber = {
  name: string
  values: (number | null)[]
  type: 'Percentage' | 'Count'
}

type DataQualityMetricText = {
  name: string
  values: (string | null)[]
  type: 'Text'
}

type DataQualityMetric = DataQualityMetricNumber | DataQualityMetricText

export type DataQualityMetricValue =
  | {
      name: string
      value: number
      displayType: 'Percentage' | 'Count'
    }
  | {
      name: string
      value: string
      displayType: 'Text'
    }

export type TextFormatOptions = 'full' | 'partial' | 'off'
</script>

<script setup lang="ts">
const props = defineProps<{ data: Data }>()
const config = useVisualizationConfig()

const INDEX_FIELD_NAME = '#'

const rowLimit = ref(0)
const page = ref(0)
const pageLimit = ref(0)
const rowCount = ref(0)
const filteredRowCount = ref(null)
const showRowCount = ref(true)
const isTruncated = ref(false)
const filterModel = ref<GridFilterModel[]>([])
const sortModel = ref<SortModel[]>([])
const hiddenColumns = ref<string[]>([])
const vizColumnOrder = ref<string[] | null>(null)
const defaultColDef: Ref<ColDef> = ref({
  editable: false,
  sortable: true,
  resizable: true,
  minWidth: 25,
  cellRenderer: cellRenderer,
  cellClass: cellClass,
  cellStyle: { 'padding-left': 0, 'border-right': '1px solid #C0C0C0' },
} satisfies ColDef)
const rowData = ref<Record<string, any>[]>([])
const columnDefs: Ref<ColDef[]> = ref([])
const nodeType = ref<ProjectPath | undefined>(undefined)
const grid = ref<
  ComponentInstance<typeof AgGridTableView> & ComponentExposed<typeof AgGridTableView>
>()

const getSvgTemplate = (icon: string) =>
  `<svg viewBox="0 0 16 16" width="16" height="16"><use xlink:href="${svgUseHref(icon)}"/></svg>`

const getContextMenuItems = (
  params: GetContextMenuItemsParams,
): (MenuItemDef | string)[] | GetContextMenuItems => {
  const colId = params.column ? params.column.getColId() : null
  const { rowIndex } = params.node ?? {}

  const actions = [
    { name: 'Get Column', action: 'at', colId, icon: 'select_column' },
    { name: 'Get Row', action: 'get_row', rowIndex, icon: 'select_row' },
    { name: 'Get Value', action: 'get_value', colId, rowIndex, icon: 'local_scope4' },
  ]

  const createMenuItem = ({ name, action, colId, rowIndex, icon }: (typeof actions)[number]) => ({
    name,
    action: () => createValueNode(colId, rowIndex, action),
    icon: getSvgTemplate(icon),
  })

  return [
    commonContextMenuActions.copy,
    commonContextMenuActions.copyWithHeaders,
    'separator',
    'export',
    ...actions.map(createMenuItem),
  ]
}

function getAstValuePattern(value?: string | number, action?: string) {
  if (action && value != null) {
    return Pattern.new<Ast.Expression>((ast) =>
      Ast.App.positional(
        Ast.PropertyAccess.new(ast.module, ast, Ast.identifier(action)!),
        typeof value === 'number' ?
          Ast.tryNumberToEnso(value, ast.module)!
        : Ast.TextLiteral.new(value, ast.module),
      ),
    )
  }
}

function getAstGetValuePattern(columnId?: string, rowIndex?: number, action?: string) {
  if (action && columnId && rowIndex != undefined) {
    const pattern = Pattern.parseExpression('__ __')
    return Pattern.new<Ast.Expression>((ast) =>
      Ast.App.positional(
        Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('get_value')!),
        pattern.instantiateCopied([
          Ast.TextLiteral.new(columnId as string, ast.module),
          Ast.tryNumberToEnso(rowIndex as number, ast.module)!,
        ]),
      ),
    )
  }
}

function createValueNode(columnId?: string | null, rowIndex?: number | null, action?: string) {
  let pattern
  if (action === 'at' && columnId != null) {
    pattern = getAstValuePattern(columnId, action)
  }
  if (action === 'get_row' && rowIndex != null) {
    pattern = getAstValuePattern(rowIndex, action)
  }
  if (action === 'get_value' && columnId != null && rowIndex != null) {
    pattern = getAstGetValuePattern(columnId, rowIndex, action)
  }

  if (pattern) {
    config.createNodes({
      content: pattern,
      commit: true,
    })
  }
}

const allRowCount = computed(() =>
  typeof props.data === 'object' && 'all_rows_count' in props.data ? props.data.all_rows_count : 0,
)

const tableVersionHash = computed(() =>
  typeof props.data === 'object' && 'table_version_hash' in props.data ?
    props.data.table_version_hash
  : null,
)

const isSSRM = computed(
  () =>
    typeof props.data === 'object' &&
    'is_using_server_sort_and_filter' in props.data &&
    props.data.is_using_server_sort_and_filter,
)

const useBottomStatusBar = computed(
  () =>
    typeof props.data === 'object' &&
    'use_bottom_status_bar' in props.data &&
    props.data.use_bottom_status_bar,
)

const isCreateNewNodeEnabled = computed(
  () =>
    typeof props.data === 'object' &&
    'enable_create_node' in props.data &&
    props.data.enable_create_node,
)

const ssrmServer = computed(() => {
  return isSSRM.value && createServer()
})

const refreshDataSource = ref(0)
const ssrmDatasource = computed(() => {
  const value = refreshDataSource.value
  return isSSRM.value && createServerSideDatasource()
})

const statusBar = computed(() => ({
  statusPanels:
    useBottomStatusBar.value ?
      [
        {
          statusPanel: TableVizStatusBar,
          statusPanelParams: {
            total: allRowCount.value,
            filtered: isSSRM.value ? filteredRowCount.value : null,
          },
        },
      ]
    : [],
}))

const isTableFilteredOrSorted = computed(
  () =>
    sortModel.value.length > 0 ||
    filterModel.value.length > 0 ||
    hiddenColumns.value.length > 0 ||
    vizColumnOrder.value != null,
)

// if there are upstream updates only to the row information the table version hash change indicates the grid needs to re get rows for any potetial changes
watch(tableVersionHash, () => {
  refreshDataSource.value++
})

watchEffect(() => {
  // if the column definitions remain the same but there has been updates upstream ag grid doesn't know to change its row model or to fetch new data
  if (
    (nodeType.value != null || config.nodeType != null) &&
    !nodeType.value?.equals(config.nodeType)
  ) {
    grid.value?.forceGridRefresh()
    nodeType.value = config.nodeType
  }
})

const textFormatterSelected = ref<TextFormatOptions>('partial')

const isRowCountSelectorVisible = computed(() => rowCount.value > 1000)
const dataGroupingMap = shallowRef<Map<string, boolean>>()

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

function setRowLimit(newRowLimit: number) {
  if (newRowLimit !== rowLimit.value) {
    rowLimit.value = newRowLimit
  }
}

watchEffect(() =>
  config.setPreprocessor(
    'Standard.Visualization.Table.Visualization',
    'prepare_visualization',
    rowLimit.value.toString(),
  ),
)

const numberFormatGroupped = new Intl.NumberFormat(undefined, {
  style: 'decimal',
  maximumSignificantDigits: 16,
  useGrouping: true,
})

const numberFormat = new Intl.NumberFormat(undefined, {
  style: 'decimal',
  maximumSignificantDigits: 16,
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

const createRowsForTable = (data: unknown[][], shift: number, isSSrm: boolean) => {
  const rows = data && data.length > 0 ? (data[0]?.length ?? 0) : 0
  const getIndexInfo = (i: number) => {
    return isSSrm ? data?.[0]?.[i] : i
  }
  return Array.from({ length: rows }, (_, i) => {
    return Object.fromEntries(
      columnDefs.value.map((h, j) => {
        return [h.field, h.field === INDEX_FIELD_NAME ? getIndexInfo(i) : data?.[j - shift]?.[i]]
      }),
    )
  })
}

async function getFilterValues(params: SetFilterValuesFuncParams) {
  const colName = params.colDef.field
  const filters = params.api.getFilterModel()
  const columnHeaders =
    typeof props.data === 'object' && 'header' in props.data ? (props.data.header ?? []) : []
  const gridFilterModelList: Array<GridFilterModel> = filters ? makeFilterModelList(filters) : []
  const { filterColumnIndexList, filterActions, valueList } = convertFilterModel(
    gridFilterModelList,
    columnHeaders,
    colTypeMap.value,
  )
  if (typeof props.data === 'object' && 'header' in props.data) {
    const index = props.data.header?.findIndex((h: string) => colName === h)
    const server = ssrmServer.value
    if (server) {
      const response = await server.getSetFilterValues(
        index!,
        filterColumnIndexList,
        filterActions,
        valueList,
      )
      if (response.success) {
        params.success(response.data)
      }
    }
  }
}

function createServer() {
  return {
    getSetFilterValues: async (
      columnIndex: number,
      filterColumnIndexList: string[] | string,
      filterActions: string[] | string,
      valueList:
        | string
        | (
            | {
                valueType: ValueTypes
                value: string
              }
            | {
                valueType: 'Mixed'
                value: ValueTypeArgumentChild[]
              }
            | {
                valueType: ValueTypes
                value: string
              }[]
          )[],
    ) => {
      const expressionFunction = createDistinctExpressionTemplate(
        'Standard.Visualization.Table.Visualization',
        'get_distinct_values_for_column',
        `${columnIndex}`,
        //column indexes that require a filter
        filterColumnIndexList as string[] | 'Nothing',
        //column actions i.e. Greater Than, Between...
        filterActions as string[] | 'Nothing',
        //values to filter on
        valueList as string[] | 'Nothing',
      )

      try {
        const response = await config.executeExpression(expressionFunction, 2000)
        return {
          success: true,
          data: response.value.distinct_vals,
        }
      } catch (error) {
        console.warn('Error loading filterValues for column.', error)
        return {
          success: false,
          data: [],
        }
      }
    },
    getData: async (request: IServerSideGetRowsRequest) => {
      const columnHeaders =
        typeof props.data === 'object' && 'header' in props.data ? (props.data.header ?? []) : []

      const { sortColIndexes, sortDirections } = convertSortModel(request, columnHeaders)
      const gridFilterModelList: Array<GridFilterModel> =
        request.filterModel ? makeFilterModelList(request.filterModel) : []
      const { filterColumnIndexList, filterActions, valueList } = convertFilterModel(
        gridFilterModelList,
        columnHeaders,
        colTypeMap.value,
      )

      const expressionFunction = createExpressionRowTemplate(
        'Standard.Visualization.Table.Visualization',
        'get_rows_for_table',
        //the index of the next bucket of rows to get
        `${request.startRow}`,
        //column indexes that require a sort
        sortColIndexes as string[] | 'Nothing',
        //direction (Ascending/Descending) for the sorts
        sortDirections as string[] | 'Nothing',
        //column indexes that require a filter
        filterColumnIndexList as string[] | 'Nothing',
        //column actions i.e Greater Than, Between...
        filterActions as string[] | 'Nothing',
        //values to filter on
        valueList as string[] | 'Nothing',
      )

      try {
        const response = await config.executeExpression(expressionFunction)
        filteredRowCount.value = response.value.row_count
        return {
          success: true,
          data: response.value.rows,
          rowCount: response.value.row_count,
        }
      } catch (error) {
        console.warn('Error loading rows for table.', error)
        return {
          success: false,
          data: null,
          rowCount: undefined,
        }
      }
    },
  }
}

interface Response {
  data: unknown[][]
  success: boolean
  rowCount: number
}
function createServerSideDatasource(): IServerSideDatasource {
  return {
    getRows: async (params) => {
      const server = ssrmServer.value
      if (server) {
        const serverResponse = await server.getData(params.request)
        const response: Response =
          serverResponse ? serverResponse : { data: [], success: false, rowCount: 0 }
        if (response.success) {
          const rows = createRowsForTable(response.data, 0, true)
          params.success({ rowData: rows, rowCount: response.rowCount })
        } else {
          params.fail()
        }
      }
    },
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
  else if (typeof params.value === 'string')
    return formatText(params.value, textFormatterSelected.value)
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
  const cellValueType = valueType ? getCellDataType(valueType.constructor) : false

  const usingMultiFilterLists =
    typeof props.data === 'object' && 'is_using_multi_filter' in props.data ?
      props.data.is_using_multi_filter
    : []
  const isUsingMultiFilter = usingMultiFilterLists[index!] ?? false

  const filterType = valueType ? getFilterType(valueType.constructor, isUsingMultiFilter) : null

  const filterParams = getFilterParams(isSSRM.value, valueType, filterType, getFilterValues)

  const dataQualityMetrics =
    typeof props.data === 'object' && 'data_quality_metrics' in props.data ?
      props.data.data_quality_metrics
        .map((metric: DataQualityMetric) => {
          const result: DataQualityMetricValue =
            metric.type === 'Text' ?
              { name: metric.name, value: metric.values[index!] || '', displayType: 'Text' }
            : { name: metric.name, displayType: metric.type, value: metric.values[index!] || 0 }
          return (
              metric.values[index!] === null ||
                (result.displayType === 'Percentage' && result.value === 0)
            ) ?
              null
            : result
        })
        .filter((obj) => obj !== null)
    : []

  const hasDataQualityMetrics = dataQualityMetrics.length > 0
  const showDataQuality =
    dataQualityMetrics.filter((obj) => obj.displayType === 'Percentage').length > 0

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

  const colDef = {
    field: name,
    headerName: name, // AGGrid would demangle it its own way if not specified.
    filter: filterType,
    filterParams: filterParams,
    headerComponentParams: {
      template,
      setAriaSort: () => {},
    },
    tooltipComponent: TableVisualisationTooltip,
    headerTooltip: displayValue ? displayValue : '',
    tooltipComponentParams: {
      dataQualityMetrics,
      total: typeof props.data === 'object' ? props.data.all_rows_count : 0,
      showDataQuality: hasDataQualityMetrics,
    },
    cellDataType: cellValueType,
    autoHeight: cellValueType === 'text' && isSSRM.value,
    sortable: valueType?.constructor !== 'Mixed',
  }
  if (valueType && ['Date', 'Date_Time', 'Time'].includes(valueType.constructor)) {
    return {
      ...colDef,
      comparator: (valueA, valueB) => {
        const textA =
          valueA && typeof valueA === 'object' && '_display_text_' in valueA ?
            valueA['_display_text_']
          : valueA
        const textB =
          valueB && typeof valueB === 'object' && '_display_text_' in valueB ?
            valueB['_display_text_']
          : valueB

        if (textA == null && textB == null) return 0
        if (textA == null) return 1
        if (textB == null) return -1

        return textA.toString().localeCompare(textB.toString())
      },
    }
  }
  return colDef
}

type ParsedActionTemplate = {
  pattern: string
  selectors: { name: string; numeric: boolean }[]
}

function parseActionTemplate(input: string, defaultSelector: string): ParsedActionTemplate {
  const regex = /{{([#@]?)(\w+)}}/g
  const selectors: { name: string; numeric: boolean }[] = []
  let pattern = input

  pattern = pattern.replace(regex, (_, flag, key) => {
    selectors.push({ name: key, numeric: flag === '#' })
    return '__'
  })

  pattern = '__.' + pattern

  // template didn't contain any {{}} placeholders so add a single default argument
  if (selectors.length === 0) {
    selectors.push({ name: defaultSelector, numeric: false })
    pattern = pattern + ' __'
  }

  return { pattern, selectors }
}

function isNumber(value: unknown): value is number {
  return typeof value === 'number' && !isNaN(value)
}

function getAstPattern(params: CellDoubleClickedEvent, action: string, defaultSelector: string) {
  const parsedAction = parseActionTemplate(action, defaultSelector)

  return Pattern.new<Ast.Expression>((ast) => {
    const mappedExpressions = parsedAction.selectors.map(({ name, numeric }) => {
      const value = params.data[name]
      const castedValue = numeric && !isNaN(Number(value)) ? Number(value) : value
      return isNumber(castedValue) ?
          Ast.tryNumberToEnso(castedValue, ast.module)!
        : Ast.TextLiteral.new(castedValue, ast.module)
    })

    const templatePattern = Pattern.parseExpression(parsedAction.pattern)
    return templatePattern.instantiateCopied([ast, ...mappedExpressions])
  })
}

/**
 * Creates a new node in the graph based on the given action template and data from a grid row.
 *
 * The action string should be of the format `at {{#fieldname}}` which will generate a Node `at 2`
 * or `at {{@fieldname}}` which will generate a Node `at "2"`
 * If the action contains no placeholders then the defaultSelector is used like so
 * `action {{@defaultSelector}}`
 * @param params - The grid cell event containing the clicked row's data.
 * @param defaultSelector - A fallback key used when the template contains no placeholders.
 * @param action - A template string with placeholders (e.g., `at {{@name}}`, `at {{#value}}`) used to generate the AST.
 */
function createNode(params: CellDoubleClickedEvent, defaultSelector: string, action: string) {
  const pattern = getAstPattern(params, action, defaultSelector)

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
  getChildAction: string
}

function toLinkField(fieldName: string, options: LinkFieldOptions): ColDef {
  const { tooltipValue, headerName, getChildAction } = options
  return {
    headerName: headerName ? headerName : fieldName,
    field: fieldName,
    onCellDoubleClicked: (params) => createNode(params, fieldName, getChildAction),
    tooltipValueGetter: (params: ITooltipParams) =>
      params.node?.rowPinned === 'top' ?
        null
      : `Double click to view this ${tooltipValue ?? 'value'} in a separate component`,
    cellRenderer: (params: ICellRendererParams) =>
      params.value !== null && params.value !== undefined ?
        `<div class='link'> ${params.value} </div>`
      : null,
    cellStyle: { 'padding-left': '13px', 'border-right': '1px solid #C0C0C0' },
    headerClass: 'indexColumnHeader',
    filter: fieldName != INDEX_FIELD_NAME,
  }
}

const DEFAULT_DATA = {
  type: typeof props.data,
  json: props.data,
  // eslint-disable-next-line camelcase
  all_rows_count: 1,
  data: undefined,
  header: undefined,
  // eslint-disable-next-line camelcase
  value_type: undefined,
  // eslint-disable-next-line camelcase
  has_index_col: false,
  links: undefined,
  // eslint-disable-next-line camelcase
  get_child_node_action: '',
  // eslint-disable-next-line camelcase
  get_child_node_link_name: undefined,
  // eslint-disable-next-line camelcase
  child_label: undefined,
  // eslint-disable-next-line camelcase
  visualization_header: undefined,
  // eslint-disable-next-line camelcase
  is_using_server_sort_and_filter: undefined,
  // eslint-disable-next-line camelcase
  requires_number_format: undefined,
}

watchEffect(() => {
  try {
    refresh()
  } catch (error) {
    console.warn('Error refreshing table.', error)
  }
})

// Update state computed from the input `data`.
function refresh() {
  // If the user switches from one visualization type to another, we can receive the raw object.
  const data_ = typeof props.data === 'object' ? props.data : DEFAULT_DATA
  if (isError(data_)) {
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
  } else if (isGenericGrid(data_)) {
    columnDefs.value = data_.headers.map((header) => {
      if (header.get_child_node_action) {
        return toLinkField(header.visualization_header, {
          tooltipValue: header.child_label,
          headerName: header.visualization_header,
          getChildAction: header.get_child_node_action,
        })
      } else {
        return toField(header.visualization_header)
      }
    })
    rowData.value = createRowsForTable(data_.data, 0, false)
  } else if (Array.isArray(data_.json)) {
    columnDefs.value = [
      toLinkField(INDEX_FIELD_NAME, {
        tooltipValue: data_.child_label,
        headerName: data_.visualization_header,
        getChildAction: data_.get_child_node_action,
      }),
      toField('Value'),
    ]
    rowData.value = data_.json.map((row, i) => ({ [INDEX_FIELD_NAME]: i, Value: row }))
    isTruncated.value = data_.all_rows_count ? data_.all_rows_count !== data_.json.length : false
  } else if (data_.json !== undefined) {
    // single values like Integer or Text
    columnDefs.value = [toField('Value')]
    rowData.value = [{ Value: data_.json }]
  } else {
    const dataHeader =
      ('header' in data_ ? data_.header : [])?.map((v, i) => {
        const valueType = data_.value_type ? data_.value_type[i] : null
        if (data_.get_child_node_link_name === v) {
          return toLinkField(v, {
            tooltipValue: data_.child_label,
            headerName: data_.visualization_header,
            getChildAction: data_.get_child_node_action,
          })
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

    if (!data_.is_using_server_sort_and_filter) {
      const shift = data_.type === 'EnsoTableOrColumn' ? 1 : 0
      rowData.value = data_.data ? createRowsForTable(data_.data, shift, false) : []
    }
  }
  const headerGroupingMap = new Map()

  const determineGrouping = (header: string) =>
    rowData.value.some((row) => {
      const value = row[header] && typeof row[header] === 'object' ? row[header].value : row[header]
      return value > 999999 || value < -999999
    })

  if ('header' in data_) {
    const headers = data_.header || []

    if (data_.requires_number_format) {
      columnDefs.value.forEach((col) => {
        const colHeader = col.headerName
        if (colHeader === INDEX_FIELD_NAME) {
          headerGroupingMap.set(INDEX_FIELD_NAME, false)
        }

        if (typeof props.data === 'object' && 'header' in props.data) {
          const dataHeaderIndex = props.data.header?.indexOf(colHeader ?? '')
          const needsGrouping =
            dataHeaderIndex !== -1 ? data_.requires_number_format[dataHeaderIndex!] : false
          headerGroupingMap.set(colHeader, needsGrouping)
        }
      })
    } else {
      headers.forEach((header) => headerGroupingMap.set(header, determineGrouping(header)))
    }
  } else {
    const headers = rowData.value[0] ? Object.keys(rowData.value[0]) : []
    Object.keys(headers).forEach((header) =>
      headerGroupingMap.set(header, determineGrouping(header)),
    )
  }

  dataGroupingMap.value = headerGroupingMap

  // Update paging
  const newRowCount = data_.all_rows_count == null ? 1 : data_.all_rows_count
  showRowCount.value = !(data_.all_rows_count == null)
  rowCount.value = newRowCount
  const newPageLimit = Math.ceil(newRowCount / rowLimit.value)
  pageLimit.value = newPageLimit
  if (page.value > newPageLimit) {
    page.value = newPageLimit
  }

  // If data is truncated, we cannot rely on sorting/filtering so will disable.
  defaultColDef.value.filter = !isTruncated.value
  defaultColDef.value.sortable = !isTruncated.value
}

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
  if (columnType === 'Mixed') {
    return (item: string, module: Ast.MutableModule) => {
      const parsedCellType = getCellValueType(item)
      return getFormattedValueForCell(item, module, parsedCellType)
    }
  }
  if (columnType === 'Boolean') {
    return (item: string, module: Ast.MutableModule) => {
      return item === 'false' ?
          Ast.Ident.new(module, Ast.identifier('False')!)
        : Ast.Ident.new(module, Ast.identifier('True')!)
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
    .filter((part, i) => i < 3)
    .map((part) => Ast.tryNumberToEnso(Number(part), module)!)
  return dateOrTimePattern.instantiateCopied([...dateTimeParts])
}

function checkSortAndFilter(e: SortChangedEvent) {
  const gridApi = e.api
  if (gridApi == null) {
    console.warn('AG Grid column API does not exist.')
    return
  }
  const colState = gridApi.getColumnState()
  const gridFilterModel = gridApi.getFilterModel()
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
  const filter = makeFilterModelList(gridFilterModel)
  if (sort.length || filter.length) {
    sortModel.value = sort as SortModel[]
    filterModel.value = filter
  } else {
    sortModel.value = []
    filterModel.value = []
  }
}

const onColumnStateChange = (e: ColumnVisibleEvent | ColumnMovedEvent) => {
  const colState = e.api.getColumnState()
  hiddenColumns.value = colState.filter((col) => col.hide).map((col) => col.colId)
  const gridColOrder = colState
    .filter((col) => col.colId != INDEX_FIELD_NAME)
    .map((col) => col.colId)
  const defaultColOrder =
    typeof props.data === 'object' && 'header' in props.data && props.data.header ?
      props.data.header
    : []
  if (gridColOrder.every((val, index) => val === defaultColOrder[index])) {
    vizColumnOrder.value = null
  } else {
    vizColumnOrder.value = gridColOrder
  }
}

const refreshGrid = () => {
  grid.value?.gridApi?.setFilterModel(null)
  grid.value?.gridApi?.resetColumnState()
}

// ===============
// === Updates ===
// ===============
onMounted(() => {
  rowLimit.value = 1000
})

// ===============
// === Toolbar ===
// ===============

config.setToolbar(
  useTableVizToolbar({
    textFormatterSelected,
    filterModel,
    sortModel,
    tableFilteredOrSorted: isTableFilteredOrSorted,
    isCreateNewNodeEnabled,
    createNodes: config.createNodes,
    getColumnValueToEnso,
    hiddenColumns,
    vizColumnOrder,
    refreshGrid,
  }),
)
</script>

<template>
  <div ref="rootNode" class="TableVisualization" @wheel.stop.passive @pointerdown.stop>
    <template v-if="!useBottomStatusBar">
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
    </template>
    <!-- TODO[ao]: Suspence in theory is not needed here (the entire visualization is inside
     suspense), but for some reason it causes reactivity loop - see https://github.com/enso-org/enso/issues/10782 -->
    <Suspense>
      <AgGridTableView
        ref="grid"
        class="scrollable grid"
        :columnDefs="columnDefs"
        :rowData="rowData"
        :defaultColDef="defaultColDef"
        :textFormatOption="textFormatterSelected"
        :datasource="ssrmDatasource"
        :rowCount="allRowCount"
        :isServerSideModel="isSSRM"
        :statusBar="statusBar"
        :gridIdHash="tableVersionHash"
        :getContextMenuItems="getContextMenuItems"
        @sortOrFilterUpdated="checkSortAndFilter"
        @columnVisibleChanged="onColumnStateChange"
        @columnMoved="onColumnStateChange"
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

:deep(.indexColumnHeader) {
  padding-left: 13px;
}
</style>
