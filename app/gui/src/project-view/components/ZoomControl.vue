<script setup lang="ts">
import ActionButton from './ActionButton.vue'
import SvgIcon from './SvgIcon.vue'

const props = defineProps<{ zoomLevel?: number }>()
</script>

<template>
  <div class="ZoomControl flex gap-1">
    <SvgIcon class="mr-1" name="zoom" /> Zoom
    <ActionButton action="graph.zoomOut" />
    <span
      class="zoomScaleLabel"
      v-text="props.zoomLevel ? props.zoomLevel.toFixed(0) + '%' : '?'"
    ></span>
    <ActionButton action="graph.zoomIn" />
    <div class="divider"></div>
    <ActionButton action="graph.fitAll" />
  </div>
</template>

<style scoped>
.ZoomControl {
  padding: 0 8px;
  align-items: center;
  border-radius: var(--radius-full);
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
}

.zoomScaleLabel {
  width: 4em;
  text-align: center;
}

.divider {
  border-left: 1px solid var(--color-text);
  border-right: 1px solid var(--color-text);
  height: 17px;
  margin-left: 8px;
  margin-right: 8px;
  opacity: 0.3;
}
</style>
