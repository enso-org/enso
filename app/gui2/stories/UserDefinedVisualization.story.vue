<script setup lang="ts">
import GraphVisualization from '@/components/GraphEditor/GraphVisualization.vue'
import { Vec2 } from '@/util/data/vec2'
import type { VisualizationIdentifier } from 'shared/yjsModel'
import { computed, ref } from 'vue'
import MockFSWrapper from './MockFSWrapper.vue'
import HstCode from './histoire/HstCode.vue'
import HstDirectory from './histoire/HstDirectory.vue'

// The `.slice()` is required to make this array mutable.
const fsSources = (
  [
    { label: 'Code Editor', value: 'editor' },
    { label: 'File System (Chromium only)', value: 'fs' },
  ] as const
).slice()

const fsSource = ref<'editor' | 'fs'>('editor')
const directory = ref<FileSystemDirectoryHandle>()
const type = ref('Example Visualization')
const isCircularMenuVisible = ref(false)

// Opening angle brackets for top-level elements escaped to avoid breaking Vue.
const code = ref(`\
\x3cscript lang="ts">
export const name = 'Example Visualization'
export const inputType = 'Any'
\x3c/script>

\x3cscript setup lang="ts">
import { VisualizationContainer } from 'builtins'
const props = defineProps<{ data: unknown }>()
\x3c/script>

\x3ctemplate>
  <VisualizationContainer :belowToolbar="true">
    <pre><code class="green-text" v-text="props.data"></code></pre>
  </VisualizationContainer>
\x3c/template>

\x3cstyle scoped>
.green-text {
  color: green;
}
\x3c/style>`)

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

const nodePosition = ref(new Vec2(0, 0))
const nodeSize = ref(new Vec2(400, 32))
const currentType = computed<VisualizationIdentifier>(() => ({
  module: { kind: 'CurrentProject' },
  name: type.value,
}))

const mockFsWrapperProps = computed(() => ({
  prefix: ['visualizations'],
  files: fsSource.value === 'editor' ? { 'ExampleVisualization.vue': code.value } : undefined,
  directory: directory.value,
}))
</script>

<template>
  <Story
    title="User Defined"
    group="visualizations"
    :layout="{ type: 'grid', width: 450 }"
    autoPropsDisabled
  >
    <div style="height: 322px">
      <MockFSWrapper v-bind="mockFsWrapperProps">
        <GraphVisualization
          :currentType="currentType"
          :data="data"
          :scale="1"
          :nodePosition="nodePosition"
          :nodeSize="nodeSize"
          :isCircularMenuVisible="isCircularMenuVisible"
          @setVisualizationId="$event.module.kind === 'CurrentProject' && (type = $event.name)"
        />
      </MockFSWrapper>
    </div>

    <template #controls>
      <HstText v-model="type" title="type" />
      <HstJson v-model="data" title="data" />
      <HstRadio v-model="fsSource" title="viz source" :options="fsSources" />
      <HstCode v-if="fsSource === 'editor'" v-model="code" title="viz code" />
      <HstDirectory
        v-if="fsSource === 'fs'"
        title="viz folder"
        @update:directory="directory = $event"
      />
    </template>
  </Story>
</template>
