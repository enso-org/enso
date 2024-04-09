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
export const widgetDefinition = defineWidget(WidgetInput.isAst, {
  priority: 1,
  score: (props, _db) =>
    props.input.value.id === injectWidgetTree().connectedSelfArgumentId ?
      Score.Perfect
    : Score.Mismatch,
})
</script>

<template>
  <SvgIcon
    class="WidgetSelfIcon icon nodeCategoryIcon"
    :name="icon"
    @click.right.stop.prevent="tree.emitOpenFullMenu()"
  />
</template>

<style scoped>
.icon {
  margin: 0 4px;
}
</style>
