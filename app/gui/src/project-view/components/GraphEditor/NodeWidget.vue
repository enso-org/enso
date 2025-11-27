<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import type { UpdateHandler, WidgetModule } from '$/providers/openedProjects/widgetRegistry'
import { WidgetInput } from '$/providers/openedProjects/widgetRegistry'
import {
  injectWidgetUsageInfo,
  provideWidgetUsageInfo,
  usageKeyForInput,
} from '@/providers/widgetUsageInfo'
import { proxyRefs } from '@/util/reactivity'
import { computed, getCurrentInstance, shallowRef, watchEffect, withCtx } from 'vue'
import { bail } from 'ydoc-shared/util/assert'

const props = defineProps<{
  input: WidgetInput
  nest?: boolean
  /**
   * Do not display "no matching widget" placeholder when a widget selection fails to resolve.
   * @default false
   */
  allowEmpty?: boolean
  /**
   * A function that intercepts and handles an update emitted by this widget. It can internally
   * call `props.updateCallback` in order to propagate it upwards, or stop propagation by either returning
   * a success or error value. If the update handler for given widget is not specified, the emitted
   * widget update is automatically propagated up the tree.
   */
  updateCallback?: UpdateHandler
}>()
defineOptions({ inheritAttrs: false })

const currentProject = useCurrentProject()
const parentUsageInfo = injectWidgetUsageInfo(true)

const usageKey = computed(() => usageKeyForInput(props.input))
const sameInputParentWidgets = computed(() =>
  parentUsageInfo?.usageKey === usageKey.value ? parentUsageInfo?.previouslyUsed : undefined,
)
const nesting = computed(() => (parentUsageInfo?.nesting ?? 0) + (props.nest === true ? 1 : 0))

const selectedWidget = shallowRef<WidgetModule<WidgetInput> | undefined>()
const isSelected = computed(() => selectedWidget.value != null)
defineExpose({ isSelected })

const updateSelection = withCtx(() => {
  const registry = currentProject.widgetRegistry.value
  selectedWidget.value = registry?.select(
    {
      input: props.input,
      nesting: nesting.value,
    },
    sameInputParentWidgets.value,
  )
}, getCurrentInstance())
watchEffect(() => updateSelection())

const updateHandler = computed(
  () =>
    props.updateCallback ??
    parentUsageInfo?.updateHandler ??
    bail('Widget tree updateCallback handler missing.'),
)

const previouslyUsed = computed(() => {
  const selected = selectedWidget.value
  if (selected == null) return sameInputParentWidgets.value
  const nextSameNodeWidgets = new Set(sameInputParentWidgets.value)
  nextSameNodeWidgets.add(selected.default)
  selected.widgetDefinition.prevent?.forEach((p) => nextSameNodeWidgets.add(p))
  return nextSameNodeWidgets
})

const inputDebugDescription = computed(
  () => Object.getPrototypeOf(props.input)?.constructor?.name ?? JSON.stringify(props.input),
)

provideWidgetUsageInfo(proxyRefs({ usageKey, nesting, updateHandler, previouslyUsed }))
</script>

<template>
  <component
    :is="selectedWidget.default"
    v-if="selectedWidget"
    v-bind="$attrs"
    :input="props.input"
    :nesting="nesting"
    :updateCallback="updateHandler"
  />
  <span
    v-else-if="!props.allowEmpty"
    :title="`No matching widget for input: ${inputDebugDescription}`"
    >🚫</span
  >
</template>
