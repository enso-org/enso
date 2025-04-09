<script setup lang="ts">
import DropdownMenu from '@/components/DropdownMenu.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { Action, ActionName } from '@/providers/action'
import ActionMenu from './ActionMenu.vue'

const open = defineModel<boolean>('open', { default: false })
const props = defineProps<{ actions: (Action | ActionName)[] }>()
</script>

<template>
  <DropdownMenu
    v-model:open="open"
    placement="bottom-start"
    class="ExtendedMenu"
    title="Additional Options"
  >
    <template #button><SvgIcon name="3_dot_menu" class="moreIcon" /></template>
    <template #menu>
      <ActionMenu :actions="props.actions" @close="open = false" />
    </template>
  </DropdownMenu>
</template>

<style scoped>
.ExtendedMenu {
  --arrow-offset: -5px;
  background: var(--color-frame-bg);
  border-radius: var(--radius-full);
  margin: 0;
}

.moreIcon {
  margin: 4px;
}
</style>
