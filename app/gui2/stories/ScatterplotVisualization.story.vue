<script setup lang="ts">
import GraphVisualization from '@/components/GraphEditor/GraphVisualization.vue'
import { Vec2 } from '@/util/vec2'
import type { VisualizationIdentifier } from 'shared/yjsModel'
import { ref } from 'vue'

const isCircularMenuVisible = ref(false)

const data = ref<any>({
  axis: {
    x: { label: 'x-axis label', scale: 'linear' },
    y: { label: 'y-axis label', scale: 'logarithmic' },
  },
  points: { labels: 'visible' },
  data: [
    { x: 0.1, y: 0.7, label: 'foo', color: '#FF0000', shape: 'circle', size: 0.2 },
    { x: 0.4, y: 0.2, label: 'baz', color: '#0000FF', shape: 'square', size: 0.3 },
  ],
})

const nodeSize = ref(new Vec2(400, 32))
const currentType: VisualizationIdentifier = {
  module: { kind: 'Builtin' },
  name: 'Scatterplot',
}
</script>

<template>
  <Story
    title="Scatterplot"
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
      <HstJson v-model="data" title="data" />
    </template>
  </Story>
</template>
