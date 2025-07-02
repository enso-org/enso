<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import { type DisplayableActionName, resolveAction } from '@/providers/action'
import { computed, toValue } from 'vue'

const { action: actionName, label } = defineProps<{
  action: DisplayableActionName
  label?: string
}>()
const action = computed(() => resolveAction(actionName))

const descriptionWithShortcut = computed(() =>
  action.value.shortcut ?
    `${toValue(action.value.description)} (${toValue(action.value.shortcut?.humanReadable)})`
  : toValue(action.value.description),
)

const iconUsed = computed(() => {
  const icon = toValue(action.value.icon)
  if (toValue(action.value.toggled)) {
    return toValue(action.value.icon_toggled) ?? icon
  }
  return icon
})
</script>

<template>
  <SvgButton
    v-if="toValue(action.available)"
    :modelValue="toValue(action.toggled)"
    :name="iconUsed"
    :disabled="!toValue(action.enabled)"
    :title="descriptionWithShortcut"
    :label="label"
    :data-testid="`action:${actionName}`"
    @activate="action.action"
  />
</template>
