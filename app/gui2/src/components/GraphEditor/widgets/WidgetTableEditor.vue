<script setup lang="ts">
import { WidgetInputIsSpecificMethodCall } from '@/components/GraphEditor/widgets/WidgetFunction.vue'
import { useTableNewArgument } from '@/components/GraphEditor/widgets/WidgetTableEditor/tableNewArgument'
import ResizeHandles from '@/components/ResizeHandles.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import '@ag-grid-community/styles/ag-grid.css'
import '@ag-grid-community/styles/ag-theme-alpine.css'
import { AgGridVue } from 'ag-grid-vue3'
import { computed, ref } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const { rowData, columnDefs } = useTableNewArgument(props.input)

// === Resizing ===

const size = ref(new Vec2(200, 50))
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
    // TODO[#10293]: This widget is not yet fully implemented, so it is temporarily disabled.
    // Change this to `Score.Perfect` or implement appropriate score method as part of next task.
    score: Score.Mismatch,
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetTableEditor" :style="widgetStyle">
    <AgGridVue class="grid" :columnDefs="columnDefs" :rowData="rowData" />
    <ResizeHandles v-model="clientBounds" bottom right />
  </div>
</template>

<style scoped>
.WidgetTableEditor {
  display: flex;
  align-items: center;
  justify-content: center;
  border-radius: var(--node-port-border-radius);
}

.grid {
  width: 100%;
  height: 100%;
}
</style>
