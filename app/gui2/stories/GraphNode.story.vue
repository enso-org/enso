<script setup lang="ts">
import GraphNode from '@/components/GraphEditor/GraphNode.vue'
import { useNavigator } from '@/composables/navigator'
import { provideGraphSelection } from '@/providers/graphSelection'
import type { Node } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { logEvent } from 'histoire/client'
import { computed, reactive, ref, watchEffect } from 'vue'
import { IdMap, type SourceRange } from '../shared/yjsModel'
import { createSetupComponent } from './histoire/utils'

const idMap = new IdMap()

const nodeBinding = ref('binding')
const nodeContent = ref('content')
const nodeX = ref(0)
const nodeY = ref(0)
const selected = ref(false)
const isLatestSelected = ref(false)
const fullscreenVis = ref(false)

const position = computed(() => new Vec2(nodeX.value, nodeY.value))

function updateContent(updates: [range: SourceRange, content: string][]) {
  let content = nodeContent.value
  for (const [[start, end], replacement] of updates) {
    content = content.slice(0, start) + replacement + content.slice(end)
  }
  nodeContent.value = content
}

const rootSpan = computed(() => Ast.parseTransitional(nodeContent.value, idMap))
const pattern = computed(() => Ast.parseTransitional(nodeBinding.value, idMap))

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
  const id = node.value.rootSpan.exprId
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
        :edited="false"
        :node="node"
        @movePosition="
          (nodeX += $event.x),
            (nodeY += $event.y),
            logEvent('movePosition', [JSON.stringify($event)])
        "
        @update:content="updateContent($event), logEvent('updateContent', [JSON.stringify($event)])"
        @update:rect="(rect) => logEvent('update:rect', JSON.stringify(rect))"
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
