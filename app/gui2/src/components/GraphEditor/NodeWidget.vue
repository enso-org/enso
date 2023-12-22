<script setup lang="ts">
import type { PortId } from '@/providers/portInfo'
import { injectWidgetRegistry, type WidgetInput } from '@/providers/widgetRegistry'
import type { WidgetConfiguration } from '@/providers/widgetRegistry/configuration'
import { injectWidgetTree } from '@/providers/widgetTree'
import {
  injectWidgetUsageInfo,
  provideWidgetUsageInfo,
  usageKeyForInput,
} from '@/providers/widgetUsageInfo'
import { Ast } from '@/util/ast'
import { computed, proxyRefs } from 'vue'

const props = defineProps<{
  input: WidgetInput
  nest?: boolean
  dynamicConfig?: WidgetConfiguration | undefined
  /**
   * A function that intercepts and handles a value update emitted by this widget. When it returns
   * `false`, the update continues to be propagated to the parent widget. When it returns `true`,
   * the update is considered handled and is not propagated further.
   */
  onUpdate?: UpdateHandler
}>()
defineOptions({
  inheritAttrs: false,
})

type UpdateHandler = (value: unknown, origin: PortId) => boolean

const registry = injectWidgetRegistry()
const tree = injectWidgetTree()
const parentUsageInfo = injectWidgetUsageInfo(true)
const usageKey = computed(() => usageKeyForInput(props.input))
const sameInputAsParent = computed(() => parentUsageInfo?.usageKey === usageKey.value)

const sameInputParentWidgets = computed(() =>
  sameInputAsParent.value ? parentUsageInfo?.previouslyUsed : undefined,
)
const nesting = computed(() => (parentUsageInfo?.nesting ?? 0) + (props.nest === true ? 1 : 0))

const selectedWidget = computed(() => {
  return registry.select(
    {
      input: props.input,
      config: props.dynamicConfig ?? undefined,
      nesting: nesting.value,
    },
    sameInputParentWidgets.value,
  )
})

const updateHandler = computed(() => {
  const nextHandler =
    parentUsageInfo?.updateHandler ?? (() => console.log('Missing update handler'))
  if (props.onUpdate != null) {
    const localHandler = props.onUpdate
    return (value: unknown, origin: PortId) => {
      const handled = localHandler(value, origin)
      if (!handled) nextHandler(value, origin)
    }
  }
  return nextHandler
})

provideWidgetUsageInfo(
  proxyRefs({
    usageKey,
    nesting,
    updateHandler,
    previouslyUsed: computed(() => {
      const nextSameNodeWidgets = new Set(sameInputParentWidgets.value)
      if (selectedWidget.value != null) {
        nextSameNodeWidgets.add(selectedWidget.value.default)
        if (selectedWidget.value.widgetDefinition.prevent) {
          for (const prevented of selectedWidget.value.widgetDefinition.prevent)
            nextSameNodeWidgets.add(prevented)
        }
      }
      return nextSameNodeWidgets
    }),
  }),
)

const spanStart = computed(() => {
  if (!(props.input instanceof Ast.Ast)) return undefined
  if (props.input.astExtended == null) return undefined
  return props.input.astExtended.span()[0] - tree.nodeSpanStart
})
</script>

<template>
  <component
    :is="selectedWidget.default"
    v-if="selectedWidget"
    ref="rootNode"
    :input="props.input"
    :config="dynamicConfig"
    :nesting="nesting"
    :data-span-start="spanStart"
    @update="updateHandler"
  />
  <span
    v-else
    :title="`No matching widget for input: ${
      Object.getPrototypeOf(props.input)?.constructor?.name ?? JSON.stringify(props.input)
    }`"
    >🚫</span
  >
</template>

<style scoped>
.whitespace {
  color: transparent;
  pointer-events: none;
  user-select: none;
  white-space: pre;
}
</style>
