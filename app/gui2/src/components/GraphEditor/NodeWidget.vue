<script setup lang="ts">
import { injectWidgetRegistry, type WidgetInput } from '@/providers/widgetRegistry'
import type { WidgetConfiguration } from '@/providers/widgetRegistry/configuration'
import { injectWidgetTree } from '@/providers/widgetTree'
import { injectWidgetUsageInfo, provideWidgetUsageInfo } from '@/providers/widgetUsageInfo'
import { AstExtended } from '@/util/ast'
import type { Opt } from '@/util/opt'
import { computed, proxyRefs, toRef } from 'vue'

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
const whitespace = computed(() =>
  parentUsageInfo?.input !== props.input && props.input instanceof AstExtended
    ? ' '.repeat(props.input.whitespaceLength() ?? 0)
    : '',
)

const sameInputParentWidgets = computed(() =>
  parentUsageInfo?.input === props.input ? parentUsageInfo?.previouslyUsed : undefined,
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
    input: toRef(props, 'input'),
    previouslyUsed: computed(() => {
      const nextSameNodeWidgets = new Set(sameInputParentWidgets.value)
      if (selectedWidget.value != null) nextSameNodeWidgets.add(selectedWidget.value)
      return nextSameNodeWidgets
    }),
    nesting,
  }),
)
const spanStart = computed(() => {
  if (!(props.input instanceof AstExtended)) return undefined
  return props.input.span()[0] - tree.nodeSpanStart - whitespace.value.length
})
</script>

<template>
  {{ whitespace
  }}<component
    :is="selectedWidget"
    v-if="selectedWidget"
    ref="rootNode"
    :input="props.input"
    :config="dynamicConfig"
    :nesting="nesting"
    :data-span-start="spanStart"
    :data-nesting="nesting"
  />
</template>
