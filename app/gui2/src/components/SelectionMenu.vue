<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'

const _props = defineProps<{
  selectedComponents: number
}>()
const emit = defineEmits<{ collapseNodes: []; toggleColorPicker: [] }>()
</script>

<template>
  <div class="SelectionMenu" @pointerdown.stop @pointerup.stop @click.stop>
    <span
      v-text="`${selectedComponents} component${selectedComponents === 1 ? '' : 's'} selected`"
    />
    <SvgIcon
      name="group"
      draggable="false"
      class="icon button"
      alt="Group components"
      @click.stop="emit('collapseNodes')"
    />
    <SvgIcon
      name="paint_palette"
      draggable="false"
      class="icon button"
      alt="Change components' colors"
      @click.stop="emit('toggleColorPicker')"
    />
  </div>
</template>

<style scoped>
.SelectionMenu {
  user-select: none;
  display: flex;
  border-radius: var(--radius-full);
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
  place-items: center;
  gap: 12px;
  padding-left: 10px;
  padding-right: 10px;
  padding-top: 4px;
  padding-bottom: 4px;
}
</style>
