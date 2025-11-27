<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { type NodeId } from '$/providers/openedProjects/graph'
import ColorRing from '@/components/ColorRing.vue'
import { injectNodeColors } from '@/providers/graphNodeColors'
import { useGraphSelection } from '@/providers/graphSelection'
import * as iter from 'enso-common/src/utilities/data/iter'
import { ref } from 'vue'

const emit = defineEmits<{
  close: []
}>()

const { getNodeColor, getNodeColors } = injectNodeColors()
const selection = useGraphSelection()
const { module, graph } = useCurrentProject()

const displayedColors = new Set<string>(
  iter.filterDefined(iter.map(selection.selected, getNodeColor)),
)
const currentColor = ref<string | undefined>(iter.tryGetSoleValue(displayedColors.values()))

const editedNodeInitialColors = new Map<NodeId, string | undefined>()

function setColor(color: string | undefined) {
  currentColor.value = color
  module.value.batchEdits(() => {
    if (color) {
      for (const node of selection.selected) {
        if (!editedNodeInitialColors.has(node))
          editedNodeInitialColors.set(node, graph.value.getNodeColorOverride(node))
        graph.value.overrideNodeColor(node, color)
      }
    } else {
      for (const [node, color] of editedNodeInitialColors.entries())
        graph.value.overrideNodeColor(node, color)
    }
  })
}

const matchableColors = getNodeColors((node) => !selection.selected.has(node))
</script>

<template>
  <div class="ColorPickerMenu">
    <ColorRing
      :modelValue="currentColor"
      :matchableColors="matchableColors"
      :initialColorAngle="0"
      @update:modelValue="setColor"
      @close="emit('close')"
    />
  </div>
</template>

<style scoped>
.ColorPickerMenu {
  width: 240px;
  height: 240px;
  display: flex;
  place-items: center;
  padding: 36px;
}
</style>
