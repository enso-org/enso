<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import type { URLString } from '@/util/data/urlString'
import type { Icon } from '@/util/iconName'

const _props = defineProps(widgetProps(widgetDefinition))
const tree = injectWidgetTree()
</script>

<script lang="ts">
export const DisplayIcon: unique symbol = Symbol('DisplayIcon')
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [DisplayIcon]?: Icon | URLString
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
      class="icon nodeCategoryIcon draggable"
      :name="$props.input[DisplayIcon]"
      @click.right.stop.prevent="tree.emitOpenFullMenu()"
    />
  </div>
</template>

<style scoped>
.WidgetIcon {
  display: flex;
}
</style>
