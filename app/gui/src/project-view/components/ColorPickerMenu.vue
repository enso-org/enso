<script setup lang="ts">
import ColorRing from '@/components/ColorRing.vue'
import { injectNodeColors } from '@/providers/graphNodeColors'
import { injectGraphSelection } from '@/providers/graphSelection'
import { useGraphStore, type NodeId } from '@/stores/graph'
import { filterDefined } from '@/util/data/iterable'
import { ref } from 'vue'
import { tryGetSoleValue } from 'ydoc-shared/util/data/iterable'

const emit = defineEmits<{
  close: []
}>()

const { getNodeColor, getNodeColors } = injectNodeColors()
const selection = injectGraphSelection()
const graphStore = useGraphStore()

const displayedColors = new Set<string>(
  filterDefined(Array.from(selection.selected, (node) => getNodeColor(node))),
)
const currentColor = ref<string | undefined>(tryGetSoleValue(displayedColors.values()))

const editedNodeInitialColors = new Map<NodeId, string | undefined>()

function setColor(color: string | undefined) {
  currentColor.value = color
  graphStore.batchEdits(() => {
    if (color) {
      for (const node of selection.selected) {
        if (!editedNodeInitialColors.has(node))
          editedNodeInitialColors.set(node, graphStore.getNodeColorOverride(node))
        graphStore.overrideNodeColor(node, color)
      }
    } else {
      for (const [node, color] of editedNodeInitialColors.entries())
        graphStore.overrideNodeColor(node, color)
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
