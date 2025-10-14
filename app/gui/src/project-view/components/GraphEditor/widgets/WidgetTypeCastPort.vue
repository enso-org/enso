<script setup lang="ts">
/**
 * Little wrapper around {@link WidgetTypeCast} that forces a port creation around the whole type casting expression.
 * Ports around type casts have `portId`s of the 'inner' expression, not the whole type cast.
 * This way aliasing analysis for connections still works as intended, but we have a nice port around the whole thing.
 * It also makes the updates affecting entire typecast, not only inner expression.
 */
import {
  defineWidget,
  Score,
  WidgetInput,
  widgetProps,
  type WidgetUpdate,
} from '$/providers/openedProjects/widgetRegistry'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const input = computed(() => {
  const portId = props.input.value.expression.id
  const input = WidgetInput.FromAstWithPortId(props.input.value, portId)
  return { ...input, [IsTypeCastKey]: true as const, forcePort: true }
})

function handleUpdate(update: WidgetUpdate) {
  if (update.portUpdate && 'value' in update.portUpdate) {
    // Make an update on the port instead of inner expression only.
    //
    // This way updates will overwrite type cast, and this is by design. If the edit would want
    // to add/keep/change typecast (like during connection), it should be already in the value.
    return props.updateCallback({
      ...update,
      portUpdate: { origin: props.input.portId, value: update.portUpdate.value },
    })
  } else {
    return props.updateCallback(update)
  }
}
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.astMatcher(Ast.TypeAnnotated),
  {
    priority: 1000,
    score: () => Score.Perfect,
  },
  import.meta.hot,
)

export const IsTypeCastKey: unique symbol = Symbol.for('WidgetInput:IsTypeCast')
declare module '$/providers/openedProjects/widgetRegistry' {
  export interface WidgetInput {
    [IsTypeCastKey]?: true | undefined
  }
}
</script>

<template>
  <NodeWidget :input="input" :updateCallback="handleUpdate" />
</template>
