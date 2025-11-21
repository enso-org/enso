<script lang="ts">
import { registerHandlers } from '@/providers/action'

/**
 * A more specialized version of AGGrid's `MenuItemDef` to simplify testing (the tests need to provide
 * only values actually used by the composable)
 */
export interface MenuItem<TData> extends MenuItemDef<TData> {
  action: (params: {
    node: { data: TData | undefined } | null
    api: { copyToClipboard: () => void; cutToClipboard: () => void; pasteFromClipboard: () => void }
  }) => void
}

const AGGRID_DEFAULT_COPY_ICON =
  '<span class="ag-icon ag-icon-copy" unselectable="on" role="presentation"></span>'
const AGGRID_DEFAULT_CUT_ICON =
  '<span class="ag-icon ag-icon-cut" unselectable="on" role="presentation"></span>'
const AGGRID_DEFAULT_PASTE_ICON =
  '<span class="ag-icon ag-icon-paste" unselectable="on" role="presentation"></span>'

/** Whether to include column headers in copied clipboard content or not. See {@link sendToClipboard}. */
const copyWithHeaders = ref(false)

export const commonContextMenuActions = {
  cut: {
    name: 'Cut',
    shortcut: gridBindings.bindings['grid.cutCells'].humanReadable,
    action: ({ api }) => {
      copyWithHeaders.value = false
      api.cutToClipboard()
    },
    icon: AGGRID_DEFAULT_CUT_ICON,
  },
  copy: {
    name: 'Copy',
    shortcut: gridBindings.bindings['grid.copyCells'].humanReadable,
    action: ({ api }) => {
      copyWithHeaders.value = false
      api.copyToClipboard()
    },
    icon: AGGRID_DEFAULT_COPY_ICON,
  },
  copyWithHeaders: {
    name: 'Copy with Headers',
    action: ({ api }) => {
      copyWithHeaders.value = true
      api.copyToClipboard()
    },
    icon: AGGRID_DEFAULT_COPY_ICON,
  },
  paste: {
    name: 'Paste',
    shortcut: gridBindings.bindings['grid.pasteCells'].humanReadable,
    action: ({ api }) => api.pasteFromClipboard(),
    icon: AGGRID_DEFAULT_PASTE_ICON,
  },
} satisfies Record<string, MenuItem<unknown>>
</script>

<script setup lang="ts" generic="TData, TValue">
/**
 * Component adding some useful logic to AGGrid table component (like keeping track of colum sizes),
 * and using common style for tables in our application.
 */
import { LINE_BOUNDARIES } from '$/utils/data/string'
import { gridBindings } from '@/bindings'
import { clipboardNodeData, writeClipboard } from '@/components/GraphEditor/graphClipboard'
import {
  parseTsvData,
  rowsToTsv,
  tableToEnsoExpression,
} from '@/components/GraphEditor/widgets/WidgetTableEditor/tableParsing'
import type { TextFormatOptions } from '@/components/visualizations/TableVisualization.vue'
import {
  default as VueComponentHost,
  VueHostInstance,
  type VueComponentHandle,
} from '@/components/VueHostRender.vue'
import { modKey } from '@/composables/events'
import { useAutoBlur } from '@/util/autoBlur'
import type {
  CellEditingStartedEvent,
  CellEditingStoppedEvent,
  ColDef,
  ColGroupDef,
  ColumnMovedEvent,
  ColumnResizedEvent,
  ColumnVisibleEvent,
  FirstDataRenderedEvent,
  GetContextMenuItems,
  GetContextMenuItemsParams,
  GetRowIdFunc,
  GridApi,
  GridReadyEvent,
  ICellEditorComp,
  IHeaderComp,
  IHeaderParams,
  IServerSideDatasource,
  MenuItemDef,
  ProcessDataFromClipboardParams,
  RowDataUpdatedEvent,
  RowEditingStartedEvent,
  RowEditingStoppedEvent,
  RowHeightParams,
  SortChangedEvent,
} from 'ag-grid-enterprise'
import * as iter from 'enso-common/src/utilities/data/iter'
import * as objects from 'enso-common/src/utilities/data/object'
import {
  computed,
  h,
  reactive,
  ref,
  shallowRef,
  watch,
  type Component,
  type ComponentInstance,
} from 'vue'

const props = defineProps<{
  rowData: TData[]
  columnDefs: (ColDef<TData, TValue> | ColGroupDef<TData>)[] | null
  defaultColDef: ColDef<TData>
  getRowId?: GetRowIdFunc<TData>
  components?: Record<string, Component>
  singleClickEdit?: boolean
  stopEditingWhenCellsLoseFocus?: boolean
  suppressDragLeaveHidesColumns?: boolean
  suppressMoveWhenColumnDragging?: boolean
  textFormatOption?: TextFormatOptions
  processDataFromClipboard?: (params: ProcessDataFromClipboardParams<TData>) => string[][] | null
  datasource?: IServerSideDatasource | boolean
  rowCount?: number
  isServerSideModel?: boolean
  gridIdHash?: string | null
  getContextMenuItems?: (
    params: GetContextMenuItemsParams,
  ) => (MenuItemDef | string)[] | GetContextMenuItems
}>()
const emit = defineEmits<{
  cellEditingStarted: [event: CellEditingStartedEvent]
  cellEditingStopped: [event: CellEditingStoppedEvent]
  rowEditingStarted: [event: RowEditingStartedEvent]
  rowEditingStopped: [event: RowEditingStoppedEvent]
  rowDataUpdated: [event: RowDataUpdatedEvent]
  sortOrFilterUpdated: [event: SortChangedEvent]
  columnVisibleChanged: [event: ColumnVisibleEvent]
  columnMoved: [event: ColumnMovedEvent]
}>()

const widths = reactive(new Map<string, number>())
const wrapper = ref<HTMLElement>()
const grid = ref<ComponentInstance<typeof AgGridVue>>()
const gridApi = shallowRef<GridApi<TData>>()
const popupParent = document.body
useAutoBlur(() => grid.value?.$el)

function onGridReady(event: GridReadyEvent<TData>) {
  gridApi.value = event.api
  if (rowModelType.value === 'serverSide') {
    gridApi.value.retryServerSideLoads()
  }
}

const rowModelType = computed(() => (props.isServerSideModel ? 'serverSide' : 'clientSide'))

const gridKeyIncrement = ref(0)
const gridKey = computed(() =>
  props.gridIdHash ?
    `${props.gridIdHash}-${gridKeyIncrement.value}`
  : `grid-${gridKeyIncrement.value}`,
)

const forceGridRefresh = () => {
  //when using the ag grid severSide model this forces the grid to 'refresh' and call getRows
  gridKeyIncrement.value++
}

watch(
  () => props.textFormatOption,
  () => {
    gridApi.value?.redrawRows()
    gridApi.value?.resetRowHeights()
  },
)

function updateColumnWidths(event: FirstDataRenderedEvent | RowDataUpdatedEvent) {
  if (event.api == null) {
    console.warn('AG Grid API does not exist.')
    return
  }
  const cols = event.api.getAllGridColumns().filter((c) => {
    const id = c.getColId()
    return id && !widths.has(id)
  })
  event.api.autoSizeColumns(cols)
}

function lockColumnSize(e: ColumnResizedEvent) {
  // Check if the resize is finished, and it's not from the API (which is triggered by us).
  if (!e.finished || e.source === 'api') return
  // If the user manually resized (or manually autosized) a column, we don't want to auto-size it
  // on a resize.
  if (e.source !== 'autosizeColumns') {
    for (const column of e.columns ?? []) {
      const id = column.getColId()
      if (id) widths.set(id, column.getActualWidth())
    }
  }
}

/**
 * Copy the provided TSV-formatted table data to the clipboard.
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
function sendToClipboard({ data }: { data: string }) {
  const rows = parseTsvData(data)
  if (rows == null) return
  // First row of `data` contains column names.
  const columnNames = rows[0]
  const rowsWithoutHeaders = rows.slice(1)
  const expression = tableToEnsoExpression(rowsWithoutHeaders, columnNames)
  if (expression == null) return
  const clipboardContent = copyWithHeaders.value ? rows : rowsWithoutHeaders
  return writeClipboard({
    ...clipboardNodeData([{ expression }]),
    'text/plain': rowsToTsv(clipboardContent),
  })
}

/**
 * AgGrid does not conform RFC 4180 when serializing copied cells to TSV before calling {@link sendToClipboard}.
 * We need to escape tabs, newlines and double quotes in the cell values to make
 * sure round-trip with Excel and Google Spreadsheet works.
 */
function processCellForClipboard({
  value,
  formatValue,
}: {
  value: any
  formatValue: (arg: any) => string
}) {
  if (value == null) return ''
  else if (typeof value === 'object' && '_display_text_' in value && value['_display_text_'])
    return String(value['_display_text_'])
  const formatted = formatValue(value)
  if (formatted.match(/[\t\n\r"]/)) {
    return `"${formatted.replaceAll(/"/g, '""')}"`
  }
  return formatted
}

defineExpose({ gridApi, forceGridRefresh })

// === Keybinds ===

function gridAction(action: () => void) {
  return {
    action: () => {
      if (gridApi.value?.getFocusedCell() == null) return
      action()
    },
  }
}

const actionHandlers = registerHandlers({
  'grid.cutCells': gridAction(() => {
    copyWithHeaders.value = false
    gridApi.value?.cutToClipboard()
  }),
  'grid.copyCells': gridAction(() => {
    copyWithHeaders.value = false
    gridApi.value?.copyToClipboard()
  }),
  'grid.pasteCells': gridAction(() => gridApi.value?.pasteFromClipboard()),
})

const handler = gridBindings.handler(
  objects.mapEntries(gridBindings.bindings, (actionName) => actionHandlers[actionName].action),
)

function suppressCopy(event: KeyboardEvent) {
  // Suppress the default keybindings of AgGrid, because we want to use our own handlers (and bindings),
  // and AgGrid API does not allow copy suppression.
  if (
    (event.code === 'KeyX' || event.code === 'KeyC' || event.code === 'KeyV') &&
    modKey(event) &&
    wrapper.value != null &&
    event.target != wrapper.value
  ) {
    event.stopPropagation()
    wrapper.value.dispatchEvent(new KeyboardEvent(event.type, event))
  }
}

function stopIfPrevented(event: Event) {
  // When AG Grid handles the context menu event it prevents-default, but it doesn't stop propagation.
  if (event.defaultPrevented) event.stopPropagation()
}

// === Wrapping and Hosting Vue Components ===

const vueHost = new VueHostInstance()

const mappedComponents = computed(() => {
  if (!props.components) return
  const retval: Record<string, new () => IHeaderComp | ICellEditorComp> = {}
  for (const [key, comp] of Object.entries(props.components)) {
    class ComponentWrapper implements IHeaderComp {
      private readonly container: HTMLElement = document.createElement('div')
      private handle: VueComponentHandle | undefined

      init(params: IHeaderParams) {
        this.handle = vueHost.register(h(comp, params), this.container, params.column.getColId())
      }

      getGui() {
        return this.container
      }

      refresh(params: IHeaderParams) {
        this.handle?.update(h(comp, params), this.container)
        return true
      }

      destroy() {
        this.handle?.unregister()
      }
    }
    retval[key] = ComponentWrapper
  }
  return retval
})
const DEFAULT_ROW_HEIGHT = 22
function getRowHeight(params: RowHeightParams): number {
  if (props.textFormatOption === 'off') {
    return DEFAULT_ROW_HEIGHT
  }
  const rowData = Object.values(params.data)
  const textValues = rowData.filter((r): r is string => typeof r === 'string')
  if (!textValues.length) {
    return DEFAULT_ROW_HEIGHT
  }
  const returnCharsCount = iter.map(textValues, (text) =>
    iter.count(text.matchAll(LINE_BOUNDARIES)),
  )
  const maxReturnCharsCount = iter.reduce(returnCharsCount, Math.max, 0)
  return (maxReturnCharsCount + 1) * DEFAULT_ROW_HEIGHT
}

const { AgGridVue } = await import('./AgGridTableView/AgGridVue')
</script>

<template>
  <div
    ref="wrapper"
    @keydown="handler($event) || stopIfPrevented($event)"
    @keydown.capture="suppressCopy"
    @keydown.space.stop
  >
    <AgGridVue
      v-bind="$attrs"
      ref="grid"
      :key="gridKey"
      class="ag-theme-alpine inner"
      :headerHeight="26"
      :rowModelType="rowModelType"
      :serverSideDatasource="datasource"
      :rowCount="rowCount"
      :rowData="rowModelType === 'clientSide' ? rowData : null"
      :columnDefs="columnDefs"
      :defaultColDef="defaultColDef"
      :copyHeadersToClipboard="true"
      :processCellForClipboard="processCellForClipboard"
      :sendToClipboard="sendToClipboard"
      :suppressFieldDotNotation="true"
      :cellSelection="true"
      :popupParent="popupParent"
      :components="mappedComponents"
      :singleClickEdit="singleClickEdit"
      :stopEditingWhenCellsLoseFocus="stopEditingWhenCellsLoseFocus"
      :suppressDragLeaveHidesColumns="suppressDragLeaveHidesColumns"
      :suppressMoveWhenColumnDragging="suppressMoveWhenColumnDragging"
      :processDataFromClipboard="processDataFromClipboard"
      :allowContextMenuWithControlKey="true"
      :cacheBlockSize="rowModelType === 'clientSide' ? undefined : 1000"
      :getContextMenuItems="getContextMenuItems"
      :getRowHeight="rowModelType === 'clientSide' ? getRowHeight : null"
      @gridReady="onGridReady"
      @firstDataRendered="updateColumnWidths"
      @rowDataUpdated="(updateColumnWidths($event), emit('rowDataUpdated', $event))"
      @columnResized="lockColumnSize"
      @cellEditingStarted="emit('cellEditingStarted', $event)"
      @cellEditingStopped="emit('cellEditingStopped', $event)"
      @rowEditingStarted="emit('rowEditingStarted', $event)"
      @rowEditingStopped="emit('rowEditingStopped', $event)"
      @sortChanged="emit('sortOrFilterUpdated', $event)"
      @filterChanged="emit('sortOrFilterUpdated', $event)"
      @columnVisible="emit('columnVisibleChanged', $event)"
      @columnMoved="emit('columnMoved', $event)"
      @contextmenu="stopIfPrevented"
    />
    <VueComponentHost :host="vueHost" />
  </div>
</template>

<style src="@ag-grid-community/styles/ag-grid.css" />
<style src="@ag-grid-community/styles/ag-theme-alpine.css" />
<style scoped>
.inner {
  width: 100%;
  height: 100%;
}

/*
 * FIXME: This style should apply when using this component both in visualization and in widget.
 * Right now, it appear to only have an effect on visualization, so we have a copy of it inside
 * WidgetTableEditor.
 */
.ag-theme-alpine {
  --ag-grid-size: 3px;
  --ag-list-item-height: 20px;
  --ag-foreground-color: var(--color-text);
  --ag-background-color: var(--color-visualization-bg);
  --ag-header-foreground-color: var(--color-ag-header-text);
  --ag-odd-row-background-color: color-mix(in srgb, var(--color-visualization-bg) 98%, black);
  --ag-header-background-color: var(--color-visualization-bg);
  font-family: var(--font-mono);

  :deep(.ag-header) {
    background: linear-gradient(
      to top,
      var(--ag-odd-row-background-color),
      var(--ag-background-color)
    );
  }
}
</style>
