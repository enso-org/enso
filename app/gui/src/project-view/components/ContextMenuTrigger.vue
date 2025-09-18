<script setup lang="ts">
import ContextMenu from '@/components/ContextMenu.vue'
import type { DisplayableActionName } from '@/providers/action'
import { provideActionContext } from '@/providers/actionContext'

const { actions } = defineProps<{
  actions: DisplayableActionName[]
}>()
const emit = defineEmits<{
  shown: []
  hidden: []
}>()

const ctx = provideActionContext()

function show(at: typeof ctx.openPosition) {
  ctx.openPosition = at
  emit('shown')
}

function hide() {
  ctx.openPosition = null
  emit('hidden')
}
</script>

<template>
  <div class="ContextMenuTrigger" @contextmenu.stop.prevent="show">
    <slot />
    <ContextMenu
      v-if="ctx.openPosition != null"
      ref="menuComponent"
      :actions="actions"
      :point="ctx.openPosition"
      @close="hide"
    >
      <slot name="menuElements" />
    </ContextMenu>
  </div>
</template>

<style scoped>
.ContextMenuTrigger {
  display: contents;
}
</style>
