<script setup lang="ts">
import { logEvent } from 'histoire/client'
import { computed, ref } from 'vue'

import * as Y from 'yjs'

import GraphNode from '@/components/GraphEditor/GraphNode.vue'
import { provideGraphSelection } from '@/providers/graphSelection'
import type { Node } from '@/stores/graph'
import { RawAstExtended } from '@/util/ast'
import { useNavigator } from '@/util/navigator'
import { Rect } from '@/util/rect'
import { Vec2 } from '@/util/vec2'
import { reactive, watchEffect } from 'vue'
import { IdMap, type ContentRange } from '../shared/yjsModel'
import { createSetupComponent } from './histoire/utils'

const doc = new Y.Doc()
const text = doc.getText('content')
const yIdMap = doc.getMap<Uint8Array>('idMap')

const nodeBinding = ref('binding')
const nodeContent = ref('content')
const nodeX = ref(0)
const nodeY = ref(0)
const selected = ref(false)
const isLatestSelected = ref(false)
const fullscreenVis = ref(false)

const position = computed(() => new Vec2(nodeX.value, nodeY.value))

function updateContent(updates: [range: ContentRange, content: string][]) {
  let content = nodeContent.value
  for (const [[start, end], replacement] of updates) {
    content = content.slice(0, start) + replacement + content.slice(end)
  }
  nodeContent.value = content
}
const idMap = new IdMap(yIdMap, text)

const rootSpan = computed(() => RawAstExtended.parse(nodeContent.value, idMap))
const pattern = computed(() => RawAstExtended.parse(nodeBinding.value, idMap))

const node = computed((): Node => {
  return {
    outerExprId: '' as any,
    pattern: pattern.value,
    position: position.value,
    rootSpan: rootSpan.value,
    vis: undefined,
  }
})

const mockRects = reactive(new Map())

watchEffect((onCleanup) => {
  const id = node.value.rootSpan.astId
  mockRects.set(id, Rect.Zero)
  onCleanup(() => {
    mockRects.delete(id)
  })
})

const navigator = useNavigator(ref())
const SetupStory = createSetupComponent((app) => {
  const selection = provideGraphSelection._mock([navigator, mockRects], app)
  watchEffect(() => {
    if (selected.value) {
      selection.selectAll()
    } else {
      selection.deselectAll()
    }
  })
})
</script>

<template>
  <Story title="Node" group="graph" :layout="{ type: 'grid', width: 300 }" autoPropsDisabled>
    <SetupStory />
    <div style="height: 72px; padding: 20px; padding-left: 50px">
      <GraphNode
        :node="node"
        @movePosition="
          (nodeX += $event.x),
            (nodeY += $event.y),
            logEvent('movePosition', [JSON.stringify($event)])
        "
        @updateContent="updateContent($event), logEvent('updateContent', [JSON.stringify($event)])"
        @updateExprRect="(id, rect) => logEvent('updateExprRect', [id, JSON.stringify(rect)])"
        @replaceSelection="(selected = true), logEvent('replaceSelection', [])"
        @update:selected="(selected = $event), logEvent('update:selected', [$event])"
      />
    </div>

    <template #controls>
      <HstText v-model="nodeBinding" title="node.binding" />
      <HstText v-model="nodeContent" title="node.content" />
      <HstNumber v-model="nodeX" title="node.position.x" />
      <HstNumber v-model="nodeY" title="node.position.y" />
      <HstCheckbox v-model="selected" title="selected" />
      <HstCheckbox v-model="isLatestSelected" title="isLatestSelected" />
      <HstCheckbox v-model="fullscreenVis" title="fullscreenVis" />
    </template>
  </Story>
</template>
