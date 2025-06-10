<script setup lang="ts">
/**
 * Little wrapper around {@link WidgetTypeCast} that forces a port creation around the whole type casting expression.
 * Ports around type casts have `portId`s of the 'inner' expression, not the whole type cast.
 * This way aliasing analysis for connections still works as intended, but we have a nice port around the whole thing.
 * Edits affect only the inner expression though.
 */
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))

const input = computed(() => {
  const portId = props.input.value.expression.id
  const input = WidgetInput.FromAstWithPortId(props.input.value, portId)
  return { ...input, [IsTypeCastKey]: true as const, forcePort: true }
})
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
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [IsTypeCastKey]?: true | undefined
  }
}
</script>

<template>
  <NodeWidget :input="input" />
</template>
