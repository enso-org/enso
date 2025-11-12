<script setup lang="ts">
import MenuButton from '@/components/MenuButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { resolveAction, type DisplayableActionName } from '@/providers/action'
import { computed, toValue } from 'vue'

const { action: actionName } = defineProps<{ action: DisplayableActionName }>()
const action = computed(() => resolveAction(actionName))
</script>

<template>
  <MenuButton
    v-if="toValue(action.available)"
    :data-testid="`action:${actionName}`"
    :disabled="!toValue(action.enabled)"
    class="ContextMenuEntry"
    v-bind="action.toggled != null ? { modelValue: toValue(action.toggled) } : {}"
    @click="action.action"
  >
    <SvgIcon :name="toValue(action.icon)" class="rowIcon" />
    <span v-text="toValue(action.description)" />
    <span
      v-if="toValue(action.shortcut)"
      class="shortcutHint"
      v-text="toValue(action.shortcut)?.humanReadable"
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
