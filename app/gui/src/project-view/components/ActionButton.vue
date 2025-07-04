<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import { type DisplayableActionName, resolveAction } from '@/providers/action'
import { computed, toValue } from 'vue'

const { action: actionName, label } = defineProps<{
  action: DisplayableActionName
  label?: string
  showToggledOn?: boolean
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
    :class="showToggledOn ? 'showToggledOn' : ''"
    @activate="action.action"
  />
</template>

<style scoped>
.showToggledOn {
  &.toggledOn {
    background-color: var(--color-menu-entry-selected-bg);
  }
}
</style>
