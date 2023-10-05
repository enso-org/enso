<script setup lang="ts">
import { logEvent } from 'histoire/client'
import { computed, ref } from 'vue'

import * as Y from 'yjs'

import GraphNode from '@/components/GraphNode.vue'
import { Vec2 } from '@/util/vec2'
import type { ContentRange } from '../shared/yjsModel'

const doc = new Y.Doc()
const text = doc.getText()
const dummyPosition = Y.createRelativePositionFromTypeIndex(text, 0)

const nodeBinding = ref('binding')
const nodeContent = ref('content')
const nodeX = ref(0)
const nodeY = ref(0)
const selected = ref(false)
const isLatestSelected = ref(false)

const position = computed(() => new Vec2(nodeX.value, nodeY.value))

function updateContent(updates: [range: ContentRange, content: string][]) {
  let content = nodeContent.value
  for (const [[start, end], replacement] of updates) {
    content = content.slice(0, start) + replacement + content.slice(end)
  }
  nodeContent.value = content
}
</script>

<template>
  <Story title="Node" group="graph" :layout="{ type: 'grid', width: 300 }" auto-props-disabled>
    <div style="height: 72px; padding: 20px; padding-left: 50px">
      <GraphNode
        :node="{
          binding: nodeBinding,
          content: nodeContent,
          position,
          rootSpan: { id: '' as any, kind: 0, length: nodeContent.length, children: [] },
          docRange: [dummyPosition, dummyPosition],
        }"
        :selected="selected"
        :is-latest-selected="isLatestSelected"
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
    </template>
  </Story>
</template>
