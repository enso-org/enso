<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { injectSelectionArrow } from '@/providers/selectionArrow'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { computed, onMounted, onUnmounted, ref } from 'vue'
import { assert } from 'ydoc-shared/util/assert'
import { ArgumentNameShownKey } from './WidgetArgumentName.vue'

const props = defineProps(widgetProps(widgetDefinition))

const innerInput = computed(() => ({ ...props.input }))
const info = injectSelectionArrow(true)
const teleportTarget = ref<HTMLElement | null>()
onMounted(() => {
  assert(teleportTarget.value != null, 'Element ref must be available after mounting.')
  if (info && !info.handled) {
    info.requestArrow(teleportTarget.value)
    info.handled = true
  }
})
onUnmounted(() => info && (info.handled = false))
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  [WidgetInput.isAstOrPlaceholder, WidgetInput.isToken],
  {
    priority: 105,
    score: (props) => {
      const info = injectSelectionArrow(true)
      if (info == null) return Score.Mismatch

      if (props.input.value instanceof Ast.Token && props.input.value.id === info?.id)
        return Score.Perfect
      if (props.input.value instanceof Ast.Ast && props.input.value.id === info?.id)
        return Score.Perfect
      if (props.input.portId === info?.id) return Score.Perfect

      // Show arrow for the first child of the WidgetArgumentName (value of the argument).
      // However, if we have `info.id` set, it means we should display arrow somewhere down the chain.
      if (ArgumentNameShownKey in props.input) return info.id == null ? Score.Perfect : Score.Good

      return Score.Mismatch
    },
    allowAsLeaf: false,
  },
  import.meta.hot,
)
</script>

<template>
  <div ref="teleportTarget" class="WidgetSelectionArrow">
    <NodeWidget :input="innerInput" />
  </div>
</template>

<style scoped>
.WidgetSelectionArrow {
  position: relative;
  display: flex;
  flex-direction: row;
  align-items: center;
  min-height: var(--node-port-height);
}
</style>
