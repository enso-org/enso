<script setup lang="ts">
import GraphNode from '@/components/GraphEditor/GraphNode.vue'
import { useKeyboard } from '@/composables/keyboard'
import { useNavigator } from '@/composables/navigator'
import { provideGraphSelection } from '@/providers/graphSelection'
import type { Node } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { logEvent } from 'histoire/client'
import { computed, reactive, ref, watchEffect } from 'vue'
import { type SourceRange } from 'ydoc-shared/yjsModel'
import { createSetupComponent } from './histoire/utils'

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

const innerExpr = computed(() => Ast.parse(nodeContent.value))
const pattern = computed(() => Ast.parse(nodeBinding.value))

const node = computed((): Node => {
  return {
    outerExpr: '' as any,
    colorOverride: null,
    zIndex: 1,
    pattern: pattern.value,
    position: position.value,
    prefixes: { enableRecording: undefined },
    rootExpr: innerExpr.value,
    innerExpr: innerExpr.value,
    primarySubject: undefined,
    vis: undefined,
    conditionalPorts: new Set(),
    type: 'component',
    docs: undefined,
  }
})

const mockRects = reactive(new Map())

watchEffect((onCleanup) => {
  const id = node.value.innerExpr.id
  mockRects.set(id, Rect.Zero)
  onCleanup(() => {
    mockRects.delete(id)
  })
})

const keyboard = useKeyboard()
const navigator = useNavigator(ref(), keyboard)
const allPortsEnabled = () => true
const SetupStory = createSetupComponent((app) => {
  const selection = provideGraphSelection._mock([navigator, mockRects, allPortsEnabled, {}], app)
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
        :graphNodeSelections="undefined"
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
