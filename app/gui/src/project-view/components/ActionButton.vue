<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import { computed, toValue } from 'vue'
import { Action, ActionName, injectActions } from '../providers/action'

const { action: actionOrName } = defineProps<{ action: Action | ActionName }>()
const actions = injectActions()
const action = computed(() =>
  typeof actionOrName === 'string' ? actions[actionOrName] : actionOrName,
)
const descriptionWithShortcut = computed(() =>
  action.value.shortcut ?
    `${toValue(action.value.description)} (${toValue(action.value.shortcut)})`
  : toValue(action.value.description),
)
</script>

<template>
  <ToggleIcon
    v-if="action.toggled != null"
    :modelValue="toValue(action.toggled)"
    :icon="toValue(action.icon)"
    :disabled="toValue(action.disabled)"
    :title="descriptionWithShortcut"
    @click.stop="action.action"
  />
  <SvgButton
    v-else
    :name="toValue(action.icon)"
    :disabled="toValue(action.disabled)"
    :title="descriptionWithShortcut"
    @click.stop="action.action"
  />
</template>
