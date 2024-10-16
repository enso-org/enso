<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import type { URLString } from '@/util/data/urlString'
import type { Icon } from '@/util/iconName'
import NodeWidget from '../NodeWidget.vue'

const props = defineProps(widgetProps(widgetDefinition))
const tree = injectWidgetTree()
</script>

<script lang="ts">
export const DisplayIcon: unique symbol = Symbol.for('WidgetInput:DisplayIcon')
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [DisplayIcon]?: {
      icon: Icon | URLString
      showContents?: boolean
    }
  }
}

export const widgetDefinition = defineWidget(
  DisplayIcon,
  {
    priority: 1,
    score: Score.Perfect,
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetIcon">
    <SvgIcon
      class="nodeCategoryIcon grab-handle draggable"
      :name="props.input[DisplayIcon].icon"
      @click.right.stop.prevent="tree.emitOpenFullMenu()"
    />
    <NodeWidget v-if="props.input[DisplayIcon].showContents === true" :input="props.input" />
  </div>
</template>

<style scoped>
.WidgetIcon {
  display: flex;
  flex-direction: row;
  align-items: center;
  gap: var(--widget-token-pad-unit);

  > .SvgIcon {
    margin: 0 calc((var(--node-port-height) - 16px) / 2);
    display: flex;
  }
}
</style>
