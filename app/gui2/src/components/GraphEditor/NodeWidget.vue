<script setup lang="ts">
import {
  injectWidgetRegistry,
  widgetAst,
  type WidgetConfiguration,
  type WidgetInput,
} from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import { injectWidgetUsageInfo, provideWidgetUsageInfo } from '@/providers/widgetUsageInfo'
import { computed, proxyRefs, ref, toRef } from 'vue'

const props = defineProps<{ input: WidgetInput; nest?: boolean }>()

const registry = injectWidgetRegistry()
const tree = injectWidgetTree()
const parentUsageInfo = injectWidgetUsageInfo(true)
const whitespace = computed(() =>
  parentUsageInfo?.input !== props.input
    ? ' '.repeat(widgetAst(props.input)?.whitespaceLength() ?? 0)
    : '',
)

// TODO: Fetch dynamic widget config from engine. [#8260]
const dynamicConfig = ref<WidgetConfiguration>()
const sameInputParentWidgets = computed(() =>
  parentUsageInfo?.input === props.input ? parentUsageInfo?.previouslyUsed : undefined,
)
const nesting = computed(() => (parentUsageInfo?.nesting ?? 0) + (props.nest === true ? 1 : 0))

const selectedWidget = computed(() => {
  return registry.select(
    { input: props.input, config: dynamicConfig.value, nesting: nesting.value },
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
  const ast = widgetAst(props.input)
  return ast && ast.span()[0] - tree.nodeSpanStart - whitespace.value.length
})
</script>

<template>
  {{ whitespace
  }}<component
    :is="selectedWidget"
    ref="rootNode"
    :input="props.input"
    :config="dynamicConfig"
    :nesting="nesting"
    :data-span-start="spanStart"
    :data-nesting="nesting"
  />
</template>
