<script setup lang="ts">
import ActionMenu from '@/components/ActionMenu.vue'
import DropdownMenu from '@/components/DropdownMenu.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import type { DisplayableActionName } from '@/providers/action'
import ZoomControl from './ZoomControl.vue'

const open = defineModel<boolean>('open', { default: false })
defineProps<{
  actions: DisplayableActionName[]
  zoomControls?: { zoomLevel: number } | undefined
}>()
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
      <ActionMenu :actions="actions" @close="open = false">
        <ZoomControl v-if="zoomControls" :zoomLevel="zoomControls.zoomLevel" />
      </ActionMenu>
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
