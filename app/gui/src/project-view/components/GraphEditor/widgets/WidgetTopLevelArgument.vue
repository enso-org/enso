<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { defineWidget, Score, widgetProps } from '@/providers/widgetRegistry'
import { ApplicationKind, ArgumentInfoKey } from '@/util/callTree'

const props = defineProps(widgetProps(widgetDefinition))
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  ArgumentInfoKey,
  {
    priority: -1,
    score: (props) =>
      props.nesting < 2 && props.input[ArgumentInfoKey].appKind === ApplicationKind.Prefix ?
        Score.Perfect
      : Score.Mismatch,
  },
  import.meta.hot,
)

/** If the element is the recursively-first-child of a top-level argument, return the top-level argument element. */
export function enclosingTopLevelArgument(
  element: HTMLElement | undefined,
  rootElement: HTMLElement | undefined,
): HTMLElement | undefined {
  return (
    element?.dataset.topLevelArgument !== undefined ? element
    : !element || element === rootElement || element.parentElement?.firstElementChild !== element ?
      undefined
    : enclosingTopLevelArgument(element.parentElement, rootElement)
  )
}
</script>

<template>
  <div class="WidgetTopLevelArgument widgetResetPadding" data-top-level-argument>
    <NodeWidget :input="props.input" />
  </div>
</template>

<style scoped>
.WidgetTopLevelArgument {
  display: flex;
  flex-direction: row;
  place-items: center;
  overflow-x: clip;

  &:before {
    content: '';
    display: block;
    align-self: stretch;
    margin-top: -4px;
    margin-bottom: -4px;
    margin-right: var(--widget-token-pad-unit);
    border-left: 1px solid rgb(0 0 0 / calc(0.12 * var(--size-transition-progress, 1)));
  }
}
</style>
