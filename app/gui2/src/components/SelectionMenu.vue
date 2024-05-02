<script setup lang="ts">
import ColorRing from '@/components/ColorRing.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import { injectNodeColors } from '@/providers/graphNodeColors'
import { injectGraphSelection } from '@/providers/graphSelection'
import { computed } from 'vue'

const showColorPicker = defineModel<boolean>('showColorPicker', { required: true })
const _props = defineProps<{ selectedComponents: number }>()
const emit = defineEmits<{
  collapseNodes: []
  setNodeColor: [color: string]
  removeNodes: []
}>()

const { getNodeColor, visibleNodeColors } = injectNodeColors()
const selection = injectGraphSelection(true)
const selectionColor = computed(() => {
  if (!selection) return undefined
  let color: string | undefined = undefined
  for (const node of selection.selected) {
    const nodeColor = getNodeColor(node)
    if (nodeColor) {
      if (color !== undefined && color !== nodeColor) return undefined
      else color = nodeColor
    }
  }
  return color
})
</script>

<template>
  <div class="SelectionMenu" @pointerdown.stop @pointerup.stop @click.stop>
    <span
      v-text="`${selectedComponents} component${selectedComponents === 1 ? '' : 's'} selected`"
    />
    <SvgIcon
      name="group"
      draggable="false"
      class="icon button"
      alt="Group components"
      @click.stop="emit('collapseNodes')"
    />
    <ToggleIcon
      v-model="showColorPicker"
      :alt="`${showColorPicker ? 'Hide' : 'Show'} the component color chooser`"
      icon="paint_palette"
      class="toggle button"
      :class="{
        // Any `pointerdown` event outside the color picker will close it. Ignore clicks that occur while the color
        // picker is open, so that it isn't toggled back open.
        disableInput: showColorPicker,
      }"
    />
    <SvgIcon
      name="trash"
      draggable="false"
      class="icon button"
      alt="Delete components"
      @click.stop="emit('removeNodes')"
    />
    <div v-if="showColorPicker" class="colorPickerContainer">
      <ColorRing
        :modelValue="selectionColor"
        :matchableColors="visibleNodeColors"
        @close="showColorPicker = false"
        @update:modelValue="emit('setNodeColor', $event)"
      />
    </div>
  </div>
</template>

<style scoped>
.SelectionMenu {
  user-select: none;
  display: flex;
  border-radius: var(--radius-full);
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
  place-items: center;
  gap: 12px;
  padding-left: 10px;
  padding-right: 10px;
  padding-top: 4px;
  padding-bottom: 4px;
}

.colorPickerContainer {
  position: absolute;
  top: 36px;
  left: 0;
  width: 240px;
  height: 240px;
  display: flex;
  border-radius: var(--radius-default);
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
  place-items: center;
  padding: 36px;
}

.toggle {
  opacity: 0.6;
}

.toggledOn {
  opacity: unset;
}

.disableInput {
  pointer-events: none;
}
</style>
