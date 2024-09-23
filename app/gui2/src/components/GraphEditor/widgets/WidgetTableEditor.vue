<script setup lang="ts">
import { WidgetInputIsSpecificMethodCall } from '@/components/GraphEditor/widgets/WidgetFunction.vue'
import TableHeader from '@/components/GraphEditor/widgets/WidgetTableEditor/TableHeader.vue'
import {
  tableNewCallMayBeHandled,
  useTableNewArgument,
  type RowData,
} from '@/components/GraphEditor/widgets/WidgetTableEditor/tableNewArgument'
import ResizeHandles from '@/components/ResizeHandles.vue'
import AgGridTableView from '@/components/shared/AgGridTableView.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { useGraphStore } from '@/stores/graph'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import '@ag-grid-community/styles/ag-grid.css'
import '@ag-grid-community/styles/ag-theme-alpine.css'
import type { CellEditingStartedEvent, CellEditingStoppedEvent, Column } from 'ag-grid-enterprise'
import { computed, ref } from 'vue'
import type { ComponentExposed } from 'vue-component-type-helpers'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const suggestionDb = useSuggestionDbStore()
const grid = ref<ComponentExposed<typeof AgGridTableView<RowData, any>>>()

const { rowData, columnDefs } = useTableNewArgument(
  () => props.input,
  graph,
  suggestionDb.entries,
  props.onUpdate,
)

// === Edit Handlers ===

class CellEditing {
  handler: WidgetEditHandler
  editedCell: { rowIndex: number; colKey: Column<RowData> } | undefined
  supressNextStopEditEvent: boolean = false

  constructor() {
    this.handler = WidgetEditHandler.New('WidgetTableEditor.cellEditHandler', props.input, {
      cancel() {
        grid.value?.gridApi?.stopEditing(true)
      },
      end() {
        grid.value?.gridApi?.stopEditing(false)
      },
      suspend: () => {
        return {
          resume: () => {
            this.editedCell && grid.value?.gridApi?.startEditingCell(this.editedCell)
          },
        }
      },
    })
  }

  cellEditedInGrid(event: CellEditingStartedEvent) {
    this.editedCell =
      event.rowIndex != null ? { rowIndex: event.rowIndex, colKey: event.column } : undefined
    if (!this.handler.isActive()) {
      this.handler.start()
    }
  }

  cellEditingStoppedInGrid(event: CellEditingStoppedEvent) {
    if (!this.handler.isActive()) return
    if (this.supressNextStopEditEvent && this.editedCell) {
      this.supressNextStopEditEvent = false
      // If row data changed, the editing will be stopped, but we want to continue it.
      grid.value?.gridApi?.startEditingCell(this.editedCell)
    } else {
      this.handler.end()
    }
  }

  rowDataChanged() {
    if (this.handler.isActive()) {
      this.supressNextStopEditEvent = true
    }
  }
}

const cellEditHandler = new CellEditing()

class HeaderEditing {
  handler: WidgetEditHandler
  stopEditingCallback: ((cancel: boolean) => void) | undefined

  constructor() {
    this.handler = WidgetEditHandler.New('WidgetTableEditor.headerEditHandler', props.input, {
      cancel: () => {
        this.stopEditingCallback?.(true)
      },
      end: () => {
        this.stopEditingCallback?.(false)
      },
    })
  }

  headerEditedInGrid(stopCb: (cancel: boolean) => void) {
    // If another header is edited, stop it (with the old callback).
    if (this.handler.isActive()) {
      this.stopEditingCallback?.(false)
    }
    this.stopEditingCallback = stopCb
    if (!this.handler.isActive()) {
      this.handler.start()
    }
  }

  headerEditingStoppedInGrid() {
    this.stopEditingCallback = undefined
    if (this.handler.isActive()) {
      this.handler.end()
    }
  }
}

const headerEditHandler = new HeaderEditing()

// === Resizing ===

const size = ref(new Vec2(200, 150))
const graphNav = injectGraphNavigator()

const clientBounds = computed({
  get() {
    return new Rect(Vec2.Zero, size.value.scale(graphNav.scale))
  },
  set(value) {
    size.value = new Vec2(value.width / graphNav.scale, value.height / graphNav.scale)
  },
})

const widgetStyle = computed(() => {
  return {
    width: `${size.value.x}px`,
    height: `${size.value.y}px`,
  }
})

// === Column Default Definition ===

const defaultColDef = {
  editable: true,
  resizable: true,
  headerComponentParams: {
    onHeaderEditingStarted: headerEditHandler.headerEditedInGrid.bind(headerEditHandler),
    onHeaderEditingStopped: headerEditHandler.headerEditingStoppedInGrid.bind(headerEditHandler),
  },
}
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInputIsSpecificMethodCall({
    module: 'Standard.Table.Table',
    definedOnType: 'Standard.Table.Table.Table',
    name: 'new',
  }),
  {
    priority: 999,
    score: (props) => {
      if (!tableNewCallMayBeHandled(props.input.value)) return Score.Mismatch
      return Score.Perfect
    },
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetTableEditor" :style="widgetStyle">
    <Suspense>
      <AgGridTableView
        ref="grid"
        class="grid"
        :defaultColDef="defaultColDef"
        :columnDefs="columnDefs"
        :rowData="rowData"
        :getRowId="(row) => `${row.data.index}`"
        :components="{ agColumnHeader: TableHeader }"
        :singleClickEdit="true"
        :stopEditingWhenCellsLoseFocus="true"
        @keydown.enter.stop
        @keydown.arrow-left.stop
        @keydown.arrow-right.stop
        @keydown.arrow-up.stop
        @keydown.arrow-down.stop
        @keydown.backspace.stop
        @keydown.delete.stop
        @cellEditingStarted="cellEditHandler.cellEditedInGrid($event)"
        @cellEditingStopped="cellEditHandler.cellEditingStoppedInGrid($event)"
        @rowDataUpdated="cellEditHandler.rowDataChanged()"
        @pointerdown.stop
        @click.stop
      />
    </Suspense>
    <ResizeHandles v-model="clientBounds" bottom right />
  </div>
</template>

<style scoped>
.WidgetTableEditor {
  display: flex;
  align-items: center;
  justify-content: center;
  border-radius: var(--node-port-border-radius);
  position: relative;
}

.grid {
  width: 100%;
  height: 100%;
}
</style>
