<script setup lang="ts">
import ColorPickerMenu from '@/components/ColorPickerMenu.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'

const showColorPicker = defineModel<boolean>('showColorPicker', { required: true })
const _props = defineProps<{ selectedComponents: number }>()
const emit = defineEmits<{
  collapseNodes: []
  removeNodes: []
}>()
</script>

<template>
  <div class="SelectionMenu">
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
    <ColorPickerMenu v-if="showColorPicker" class="submenu" @close="showColorPicker = false" />
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

.submenu {
  position: absolute;
  top: 36px;
  left: 0;
  border-radius: var(--radius-default);
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
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
