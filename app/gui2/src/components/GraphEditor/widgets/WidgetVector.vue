<script setup lang="ts">
import NodeWidgetTree from '@/components/GraphEditor/NodeWidgetTree.vue'
import VectorWidget from '@/components/widgets/VectorWidget.vue'
import { Tree } from '@/generated/ast'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { AstExtended } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const graph = useGraphStore()
const value = computed({
  get() {
    return props.input.children().filter((child) => child.isTree())
  },
  set(value) {
    // FIXME: modify list of children
  },
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(AstExtended.isTree([Tree.Type.Array]), {
  priority: 1000,
  score: () => Score.Perfect,
})
</script>

<template>
  <VectorWidget
    v-slot="slotProps"
    v-model="value"
    :default="() => ({}) as never /* FIXME: */"
    class="WidgetVector"
    contenteditable="false"
  >
    <NodeWidgetTree :ast="slotProps.item" />
  </VectorWidget>
</template>
