<script setup lang="ts">
import { DIALOG_BACKGROUND, TEXT_STYLE } from '#/components/AriaComponents'
import ActionButton from './ActionButton.vue'
import SvgIcon from './SvgIcon.vue'

const props = defineProps<{ zoomLevel?: number }>()

const controlStyles = DIALOG_BACKGROUND({
  className: 'ZoomControl flex gap-1',
})

const textStyles = TEXT_STYLE()
</script>

<template>
  <div :class="controlStyles">
    <SvgIcon class="mr-1" name="zoom" />
    <span :class="textStyles">Zoom</span>
    <ActionButton action="graph.zoomOut" />
    <span
      :class="textStyles"
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
}

.label {
  user-select: none;
  pointer-events: none;
  flex: 1;
  display: flex;
  align-items: center;
  gap: 8px;
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
