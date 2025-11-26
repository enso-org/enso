<script setup lang="ts">
import {
  Score,
  WidgetInput,
  defineWidget,
  widgetProps,
} from '$/providers/openedProjects/widgetRegistry'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { ArgumentNameShownKey } from '@/components/GraphEditor/widgets/WidgetArgumentName.vue'
import { useMounted } from '@/composables/events'
import { injectSelectionArrow } from '@/providers/selectionArrow'
import { Ast } from '@/util/ast'
import { computed, ref } from 'vue'
import { assert } from 'ydoc-shared/util/assert'

const props = defineProps(widgetProps(widgetDefinition))

const innerInput = computed(() => ({ ...props.input }))
const info = injectSelectionArrow(true)
const teleportTarget = ref<HTMLElement | null>()
useMounted(() => {
  assert(teleportTarget.value != null, 'Element ref must be available after mounting.')
  if (info && !info.handled) {
    info.requestArrow(teleportTarget.value)
    info.handled = true
    return () => (info.handled = false)
  }
})
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
  <div ref="teleportTarget" class="WidgetSelectionArrow widgetParent">
    <NodeWidget :input="innerInput" />
  </div>
</template>

<style scoped>
.WidgetSelectionArrow {
  position: relative;
}
</style>
