<script setup lang="ts">
import {
  injectWidgetRegistry,
  type WidgetConfiguration,
  type WidgetInput,
} from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import {
  injectWidgetUsageInfo,
  provideWidgetUsageInfo,
  usageKeyForInput,
} from '@/providers/widgetUsageInfo'
import { AstExtended } from '@/util/ast'
import { computed, proxyRefs, ref } from 'vue'

const props = defineProps<{ input: WidgetInput; nest?: boolean }>()
defineOptions({
  inheritAttrs: false,
})

const registry = injectWidgetRegistry()
const tree = injectWidgetTree()
const parentUsageInfo = injectWidgetUsageInfo(true)
const usageKey = computed(() => usageKeyForInput(props.input))
const sameInputAsParent = computed(() => parentUsageInfo?.usageKey === usageKey.value)

const whitespace = computed(() =>
  !sameInputAsParent.value && props.input instanceof AstExtended
    ? ' '.repeat(props.input.whitespaceLength() ?? 0)
    : '',
)

// TODO: Fetch dynamic widget config from engine. [#8260]
const dynamicConfig = ref<WidgetConfiguration>()
const sameInputParentWidgets = computed(() =>
  sameInputAsParent.value ? parentUsageInfo?.previouslyUsed : undefined,
)
const nesting = computed(() => (parentUsageInfo?.nesting ?? 0) + (props.nest === true ? 1 : 0))

const selectedWidget = computed(() => {
  return registry.select(
    {
      input: props.input,
      config: dynamicConfig.value,
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
  if (!(props.input instanceof AstExtended)) return undefined
  return props.input.span()[0] - tree.nodeSpanStart - whitespace.value.length
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
