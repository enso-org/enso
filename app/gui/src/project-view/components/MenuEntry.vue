<script setup lang="ts">
import MenuButton from '@/components/MenuButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { computed, toValue } from 'vue'
import { Action, ActionName, injectActions } from '../providers/action'

const { action: actionOrName } = defineProps<{ action: Action | ActionName }>()
const actions = injectActions()
const action = computed(() =>
  typeof actionOrName === 'string' ? actions[actionOrName] : actionOrName,
)
</script>

<template>
  <MenuButton
    :data-testid="action.testid"
    :disabled="toValue(action.disabled)"
    class="ContextMenuEntry"
    v-bind="action.toggled != null ? { modelValue: toValue(action.toggled) } : {}"
    @click="action.action"
  >
    <SvgIcon :name="toValue(action.icon)" class="rowIcon" />
    <span v-text="toValue(action.description)" />
    <span
      v-if="toValue(action.shortcut)"
      class="shortcutHint"
      v-text="action.shortcut?.humanReadable"
    />
  </MenuButton>
</template>

<style scoped>
.ContextMenuEntry {
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
