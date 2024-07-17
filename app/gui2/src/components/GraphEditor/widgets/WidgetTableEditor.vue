<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import { Ast } from '@/util/ast'
import { computed } from 'vue'
import { WidgetInputIsSpecificMethodCall } from './WidgetFunction.vue'

const _props = defineProps(widgetProps(widgetDefinition))
const tree = injectWidgetTree()

const icon = computed(() => tree.icon)
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
  <div class="WidgetTableEditor">WidgetTableEditor</div>
</template>

<style scoped>
.WidgetTableEditor {
  color: yellow;
  display: flex;
  background: #00ff0055;
  padding: 50px;
  border-radius: 12px;
}
</style>
