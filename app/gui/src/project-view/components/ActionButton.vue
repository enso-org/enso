<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import { computed, toValue } from 'vue'
import { Action, ActionName, resolveAction } from '../providers/action'

const { action: actionOrName } = defineProps<{ action: Action | ActionName }>()
const action = computed(() => resolveAction(actionOrName))

const descriptionWithShortcut = computed(() =>
  action.value.shortcut ?
    `${toValue(action.value.description)} (${toValue(action.value.shortcut?.humanReadable)})`
  : toValue(action.value.description),
)
</script>

<template>
  <SvgButton
    :name="toValue(action.icon)"
    :disabled="toValue(action.disabled)"
    :title="descriptionWithShortcut"
    @click.stop="action.action"
  />
</template>
