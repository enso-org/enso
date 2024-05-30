<script setup lang="ts">
import NavBreadcrumbs, { type BreadcrumbItem } from '@/components/NavBreadcrumbs.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import SvgButton from './SvgButton.vue'

const props = defineProps<{
  breadcrumbs: BreadcrumbItem[]
  allowNavigationLeft: boolean
  allowNavigationRight: boolean
}>()
const emit = defineEmits<{ back: []; forward: []; breadcrumbClick: [index: number] }>()
</script>

<template>
  <div class="NavBar">
    <SvgIcon name="graph_editor" draggable="false" />
    <div class="breadcrumbs-controls">
      <SvgButton
        name="arrow_left"
        :disabled="!props.allowNavigationLeft"
        title="Back"
        @click.stop="emit('back')"
      />
      <SvgButton
        name="arrow_right"
        :disabled="!props.allowNavigationRight"
        title="Forward"
        @click.stop="emit('forward')"
      />
    </div>
    <NavBreadcrumbs :breadcrumbs="props.breadcrumbs" @selected="emit('breadcrumbClick', $event)" />
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
}
</style>
