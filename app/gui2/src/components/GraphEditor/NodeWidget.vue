<script setup lang="ts">
import { injectWidgetRegistry, type WidgetInput } from '@/providers/widgetRegistry'
import type { WidgetConfiguration } from '@/providers/widgetRegistry/configuration'
import { injectWidgetTree } from '@/providers/widgetTree'
import {
  injectWidgetUsageInfo,
  provideWidgetUsageInfo,
  usageKeyForInput,
} from '@/providers/widgetUsageInfo'
import { Ast } from '@/util/ast'
import type { Opt } from '@/util/opt'
import { computed, proxyRefs } from 'vue'

const props = defineProps<{
  input: WidgetInput
  nest?: boolean
  dynamicConfig?: Opt<WidgetConfiguration>
}>()
defineOptions({
  inheritAttrs: false,
})

const registry = injectWidgetRegistry()
const tree = injectWidgetTree()
const parentUsageInfo = injectWidgetUsageInfo(true)
const usageKey = computed(() => usageKeyForInput(props.input))
const sameInputAsParent = computed(() => parentUsageInfo?.usageKey === usageKey.value)

const whitespace = computed(() =>
  !sameInputAsParent.value && props.input instanceof Ast.Ast
    ? ' '.repeat(props.input.astExtended?.whitespaceLength() ?? 0)
    : '',
)

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
provideWidgetUsageInfo(
  proxyRefs({
    usageKey,
    nesting,
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
  return props.input.astExtended.span()[0] - tree.nodeSpanStart - whitespace.value.length
})
</script>

<template>
  {{ whitespace
  }}<component
    :is="selectedWidget.default"
    v-if="selectedWidget"
    ref="rootNode"
    :input="props.input"
    :config="dynamicConfig"
    :nesting="nesting"
    :data-span-start="spanStart"
    :data-nesting="nesting"
  />
  <span
    v-else
    :title="`No matching widget for input: ${
      Object.getPrototypeOf(props.input)?.constructor?.name ?? JSON.stringify(props.input)
    }`"
    >🚫</span
  >
</template>
