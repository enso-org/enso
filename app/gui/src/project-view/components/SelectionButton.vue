<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import { injectSelectionButtons, type SelectionButtons } from '@/providers/selectionButtons'

const { button: buttonName } = defineProps<{ button: keyof SelectionButtons }>()

const { buttons } = injectSelectionButtons()
const button = buttons[buttonName]
</script>

<template>
  <ToggleIcon
    v-if="button.state != null"
    v-model="button.state"
    :icon="button.icon"
    :disabled="button.disabled"
    :title="button.descriptionWithShortcut"
    @click.stop="button.action ?? ''"
  />
  <SvgButton
    v-else
    :name="button.icon"
    :disabled="button.disabled"
    :title="button.descriptionWithShortcut"
    @click.stop="button.action"
  />
</template>
