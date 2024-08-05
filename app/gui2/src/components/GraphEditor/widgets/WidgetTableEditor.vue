<script setup lang="ts">
import { WidgetInputIsSpecificMethodCall } from '@/components/GraphEditor/widgets/WidgetFunction.vue'
import TableHeader from '@/components/GraphEditor/widgets/WidgetTableEditor/TableHeader.vue'
import {
  useTableNewArgument,
  type RowData,
} from '@/components/GraphEditor/widgets/WidgetTableEditor/tableNewArgument'
import ResizeHandles from '@/components/ResizeHandles.vue'
import AgGridTableView from '@/components/widgets/AgGridTableView.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { useGraphStore } from '@/stores/graph'
import { targetIsOutside } from '@/util/autoBlur'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import '@ag-grid-community/styles/ag-grid.css'
import '@ag-grid-community/styles/ag-theme-alpine.css'
import { unrefElement } from '@vueuse/core'
import { computed, ref, watch, type ComponentInstance } from 'vue'
import type { ComponentExposed } from 'vue-component-type-helpers'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
// TODO explain
const grid = ref<
  ComponentExposed<typeof AgGridTableView<RowData, any>> &
    ComponentInstance<typeof AgGridTableView<RowData, any>>
>()

const { rowData, columnDefs } = useTableNewArgument(() => props.input, graph, props.onUpdate)

watch(rowData, (rowData) => console.log(rowData), { flush: 'sync' })
watch(columnDefs, (columnDefs) => console.log(columnDefs), { flush: 'sync' })

const editHandler = WidgetEditHandler.New('WidgetTableEditor', props.input, {
  cancel() {
    grid.value?.gridApi?.stopEditing(true)
  },
  end() {
    grid.value?.gridApi?.stopEditing(false)
  },
  pointerdown(event) {
    if (targetIsOutside(event, unrefElement(grid))) editHandler.end()
    return false
  },
})

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
    score: Score.Perfect,
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetTableEditor" :style="widgetStyle">
    <AgGridTableView
      ref="grid"
      class="grid"
      :defaultColDef="{
        editable: true,
        resizable: true,
        headerComponentParams: {
          onNameChanged: (colId: string, newName: string) =>
            console.log('Received', colId, newName),
        },
      }"
      :columnDefs="columnDefs"
      :rowData="rowData"
      :getRowId="(row) => `${row.data.index}`"
      :components="{ agColumnHeader: TableHeader }"
      @keydown.enter.stop
      @keydown.escape.stop
      @cellEditingStarted="editHandler.start()"
      @cellEditingStopped="editHandler.end()"
      @rowEditingStarted="editHandler.start()"
      @rowEditingStopped="editHandler.end()"
      @pointerdown.stop
      @click.stop
    />
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
