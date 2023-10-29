<script setup lang="ts">
import GraphVisualization from '@/components/GraphEditor/GraphVisualization.vue'
import { Vec2 } from '@/util/vec2'
import type { VisualizationIdentifier } from 'shared/yjsModel'
import { ref } from 'vue'

const isCircularMenuVisible = ref(false)

const data = ref<any>({
  latitude: 37.8,
  longitude: -122.45,
  zoom: 15,
  controller: true,
  showingLabels: true, // Enables presenting labels when hovering over a point.
  layers: [
    {
      type: 'Scatterplot_Layer',
      data: [
        {
          latitude: 37.8,
          longitude: -122.45,
          color: [255, 0, 0],
          radius: 100,
          label: 'an example label',
        },
      ],
    },
  ],
})

const nodeSize = ref(new Vec2(400, 32))
const currentType: VisualizationIdentifier = {
  module: { kind: 'Builtin' },
  name: 'Geo Map',
}
</script>

<template>
  <Story
    title="Geo Map"
    group="visualizations"
    :layout="{ type: 'grid', width: 400 }"
    autoPropsDisabled
  >
    <GraphVisualization
      :currentType="currentType"
      :data="data"
      :nodeSize="nodeSize"
      :isCircularMenuVisible="isCircularMenuVisible"
    />

    <template #controls>
      <HstCheckbox v-model="isCircularMenuVisible" title="circular menu visible" />
      <HstJson v-model="data" title="data" />
    </template>
  </Story>
</template>
