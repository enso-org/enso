<script setup lang="ts">
import SelectionBrush from '@/components/SelectionBrush.vue'
import SelectionBrushWrapper from './SelectionBrushWrapper.vue'
</script>

<template>
  <Story
    title="Selection Brush"
    group="graph"
    :layout="{ type: 'single', iframe: false }"
    responsiveDisabled
    autoPropsDisabled
  >
    <SelectionBrushWrapper>
      <template #default="{ scaledMousePos, scaledSelectionAnchor, navigator }">
        <SelectionBrush
          v-if="scaledMousePos"
          :position="scaledMousePos"
          :anchor="scaledSelectionAnchor"
          :style="{ transform: navigator.prescaledTransform }"
        />
      </template>
    </SelectionBrushWrapper>
  </Story>
</template>
