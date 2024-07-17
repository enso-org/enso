<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import { computed } from 'vue'

const _props = defineProps(widgetProps(widgetDefinition))
const tree = injectWidgetTree()

const icon = computed(() => tree.icon)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.isAst,
  {
    priority: 1,
    score: (props) =>
      props.input.value.id === injectWidgetTree().potentialSelfArgumentId ?
        Score.Perfect
      : Score.Mismatch,
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetSelfIcon">
    <SvgIcon
      class="icon nodeCategoryIcon draggable"
      :name="icon"
      @click.right.stop.prevent="tree.emitOpenFullMenu()"
    />
  </div>
</template>

<style scoped>
.WidgetSelfIcon {
  display: flex;
}
</style>
