<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import { type ActionName, resolveAction } from '@/providers/action'
import { computed, toValue } from 'vue'

const { action: actionName, label } = defineProps<{
  action: ActionName
  label?: string
}>()
const action = computed(() => resolveAction(actionName))

const descriptionWithShortcut = computed(() =>
  action.value.shortcut ?
    `${toValue(action.value.description)} (${toValue(action.value.shortcut?.humanReadable)})`
  : toValue(action.value.description),
)
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
