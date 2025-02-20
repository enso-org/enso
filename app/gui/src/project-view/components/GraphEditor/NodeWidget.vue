<script setup lang="ts">
import type { WidgetModule } from '@/providers/widgetRegistry'
import { injectWidgetRegistry, WidgetInput, type WidgetUpdate } from '@/providers/widgetRegistry'
import {
  injectWidgetUsageInfo,
  provideWidgetUsageInfo,
  usageKeyForInput,
} from '@/providers/widgetUsageInfo'
import { computed, getCurrentInstance, proxyRefs, shallowRef, watchEffect, withCtx } from 'vue'

const props = defineProps<{
  input: WidgetInput
  nest?: boolean
  allowEmpty?: boolean
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

type UpdateHandler = (update: WidgetUpdate) => boolean

const registry = injectWidgetRegistry()
const parentUsageInfo = injectWidgetUsageInfo(true)
const usageKey = computed(() => usageKeyForInput(props.input))
const sameInputAsParent = computed(() => parentUsageInfo?.usageKey === usageKey.value)

const sameInputParentWidgets = computed(() =>
  sameInputAsParent.value ? parentUsageInfo?.previouslyUsed : undefined,
)
const nesting = computed(() => (parentUsageInfo?.nesting ?? 0) + (props.nest === true ? 1 : 0))

const selectedWidget = shallowRef<WidgetModule<WidgetInput> | undefined>()
const updateSelection = withCtx(() => {
  selectedWidget.value = registry.select(
    {
      input: props.input,
      nesting: nesting.value,
    },
    sameInputParentWidgets.value,
  )
}, getCurrentInstance())
watchEffect(() => updateSelection())
const updateHandler = computed(() => {
  const nextHandler =
    parentUsageInfo?.updateHandler ?? (() => console.log('Missing update handler'))
  if (props.onUpdate != null) {
    const localHandler = props.onUpdate
    return (payload: WidgetUpdate) => {
      const handled = localHandler(payload)
      if (!handled) nextHandler(payload)
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
</script>

<template>
  <component
    :is="selectedWidget.default"
    v-if="selectedWidget"
    v-bind="$attrs"
    :input="props.input"
    :nesting="nesting"
    :data-port="props.input.portId"
    @update="updateHandler"
  />
  <span
    v-else-if="!props.allowEmpty"
    :title="`No matching widget for input: ${
      Object.getPrototypeOf(props.input)?.constructor?.name ?? JSON.stringify(props.input)
    }`"
    >🚫</span
  >
</template>
