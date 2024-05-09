<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { injectSelectionArrow } from '@/providers/selectionArrow'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { assert } from 'shared/util/assert'
import { onMounted, onUnmounted, ref } from 'vue'
import { ArgumentNameShownKey } from './WidgetArgumentName.vue'

const props = defineProps(widgetProps(widgetDefinition))

const innerInput = { ...props.input }
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
  WidgetInput.isAstOrPlaceholder,
  {
    priority: 105,
    score: (props) => {
      const info = injectSelectionArrow(true)
      if (info == null) return Score.Mismatch

      // This is needed because the id of the rhs in PropertyAccess chain is TokenId, not AstId.
      if (props.input.value instanceof Ast.Ident && props.input.value.token.id === info?.id)
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
    <NodeWidget :input="innerInput" allowEmpty />
  </div>
</template>

<style scoped>
.WidgetSelectionArrow {
  position: relative;
  display: flex;
  flex-direction: row;
  align-items: center;
  min-height: --node-port-height;
}
</style>
