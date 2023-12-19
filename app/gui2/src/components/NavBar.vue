<script setup lang="ts">
import NavBreadcrumbs from '@/components/NavBreadcrumbs.vue'
import SvgIcon from '@/components/SvgIcon.vue'

const props = defineProps<{ breadcrumbs: string[] }>()
const emit = defineEmits<{ back: []; forward: []; breadcrumbClick: [index: number] }>()
</script>

<template>
  <div class="NavBar">
    <SvgIcon name="graph_editor" draggable="false" class="icon" />
    <div class="breadcrumbs-controls">
      <SvgIcon
        name="arrow_left"
        draggable="false"
        class="icon button inactive"
        @pointerdown="emit('back')"
      />
      <SvgIcon
        name="arrow_right"
        draggable="false"
        class="icon button"
        @pointerdown="emit('forward')"
      />
    </div>
    <NavBreadcrumbs
      :breadcrumbs="props.breadcrumbs"
      @pointerdown="emit('breadcrumbClick', $event)"
    />
  </div>
</template>

<style scoped>
.NavBar {
  user-select: none;
  display: flex;
  border-radius: var(--radius-full);
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
  place-items: center;
  gap: 12px;
  padding-left: 8px;
  padding-right: 10px;
  padding-top: 4px;
  padding-bottom: 4px;

  > .breadcrumbs-controls {
    display: flex;
  }

  & .inactive {
    opacity: 0.4;
  }
}
</style>
