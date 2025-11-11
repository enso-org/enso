<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import { resolveAction, type DisplayableActionName } from '@/providers/action'
import { computed, toValue } from 'vue'

const { action: actionName, label } = defineProps<{
  action: DisplayableActionName
  label?: string
}>()
const action = computed(() => resolveAction(actionName))

const descriptionWithShortcut = computed(() => {
  const description = toValue(action.value.description)
  const shortcut = toValue(action.value.shortcut)
  return shortcut ? `${description} (${shortcut.humanReadable})` : description
})
</script>

<template>
  <SvgButton
    v-if="toValue(action.available)"
    :modelValue="toValue(action.toggled)"
    :name="toValue(action.icon)"
    :disabled="!toValue(action.enabled)"
    :title="descriptionWithShortcut"
    :label="label"
    :data-testid="`action:${actionName}`"
    @activate="action.action"
  />
</template>
