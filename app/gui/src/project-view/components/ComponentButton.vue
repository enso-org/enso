<script setup lang="ts">
import MenuButton from '@/components/MenuButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import {
  injectComponentAndSelectionButtons,
  type ComponentAndSelectionButtons,
} from '@/providers/selectionButtons'

const { button: buttonName } = defineProps<{ button: keyof ComponentAndSelectionButtons }>()

const { buttons } = injectComponentAndSelectionButtons()
const button = buttons[buttonName]
</script>

<template>
  <MenuButton
    :data-testid="button.testid"
    :disabled="button.disabled"
    class="ComponentButton"
    v-bind="button.state != null ? { modelValue: button.state } : {}"
    @update:modelValue="button.state != null && (button.state = $event)"
    @click="button.action"
  >
    <SvgIcon :name="button.icon" class="rowIcon" />
    <span v-text="button.description" />
    <span v-if="button.shortcut" class="shortcutHint" v-text="button.shortcut" />
  </MenuButton>
</template>

<style scoped>
.ComponentButton {
  display: flex;
  align-items: center;
  justify-content: left;
  padding-left: 8px;
  padding-right: 8px;
}

.rowIcon {
  display: inline-block;
  margin-right: 8px;
}

.shortcutHint {
  margin-left: auto;
  padding-left: 2em;
  opacity: 0.8;
}
</style>
