<script setup lang="ts">
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import SmallPlusButton from '@/components/SmallPlusButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import { Vec2 } from '@/util/data/vec2'
import ColorPicker from '@/components/ColorPicker.vue'
import { ref } from 'vue'

const props = defineProps<{
  isRecordingEnabledGlobally: boolean
  isRecordingOverridden: boolean
  isDocsVisible: boolean
  isVisualizationVisible: boolean
  isFullMenuVisible: boolean
  nodeColor: string
}>()
const emit = defineEmits<{
  'update:isRecordingOverridden': [isRecordingOverridden: boolean]
  'update:isDocsVisible': [isDocsVisible: boolean]
  'update:isVisualizationVisible': [isVisualizationVisible: boolean]
  startEditing: []
  startEditingComment: []
  openFullMenu: []
  delete: []
  createNode: [options: NodeCreationOptions]
  overrideColor: [value: string]
}>()
const showColorPicker = ref(false)
</script>

<template>
  <div class="CircularMenu" @pointerdown.stop @pointerup.stop @click.stop>
    <div class="circle" :class="`${props.isFullMenuVisible ? 'full' : 'partial'}`">
      <div v-if="!isFullMenuVisible" class="More" @pointerdown.stop="emit('openFullMenu')"></div>
      <SvgIcon
        v-if="isFullMenuVisible"
        name="comment"
        class="icon-container button slot2"
        :alt="`Edit comment`"
        @click.stop="emit('startEditingComment')"
      />
      <SvgIcon
        v-if="isFullMenuVisible"
        name="paint_palette"
        class="icon-container button slot3"
        :alt="`Choose color`"
        @click.stop="showColorPicker = !showColorPicker"
      />
      <SvgIcon
        v-if="isFullMenuVisible"
        name="trash2"
        class="icon-container button slot4"
        :alt="`Delete component`"
        @click.stop="emit('delete')"
      />
      <ToggleIcon
        icon="eye"
        class="icon-container button slot5"
        :alt="`${props.isVisualizationVisible ? 'Hide' : 'Show'} visualization`"
        :modelValue="props.isVisualizationVisible"
        @update:modelValue="emit('update:isVisualizationVisible', $event)"
      />
      <SvgIcon
        name="edit"
        class="icon-container button slot6"
        data-testid="edit-button"
        @click.stop="emit('startEditing')"
      />
      <ToggleIcon
        icon="record"
        class="icon-container button slot7"
        data-testid="overrideRecordingButton"
        :class="{ 'recording-overridden': props.isRecordingOverridden }"
        :alt="`${props.isRecordingOverridden ? 'Disable' : 'Enable'} recording`"
        :modelValue="props.isRecordingOverridden"
        @update:modelValue="emit('update:isRecordingOverridden', $event)"
      />
    </div>
    <SmallPlusButton
      v-if="!isVisualizationVisible"
      class="below-slot5"
      @createNode="emit('createNode', $event)"
    />
  </div>
  <ColorPicker class="colorPicker" :show="showColorPicker" :color="props.nodeColor" @update:color="(c) => emit('overrideColor', c)" />
</template>

<style scoped>
.CircularMenu {
  position: absolute;
  user-select: none;
  pointer-events: none;
  z-index: 100;
}

.colorPicker {
  position: absolute;
  top: -150px;
  left: -300px;
}

.circle {
  position: relative;
  left: -36px;
  top: -36px;
  width: 114px;
  height: 114px;

  > * {
    pointer-events: all;
  }

  &:before {
    content: '';
    position: absolute;
    backdrop-filter: var(--blur-app-bg);
    background: var(--color-app-bg);
    width: 100%;
    height: 100%;
    pointer-events: all;
  }

  &.partial {
    &:before {
      top: 36px;
      clip-path: path(
        'm0 16a52 52 0 0 0 52 52a16 16 0 0 0 0 -32a20 20 0 0 1-20-20a16 16 0 0 0-32 0'
      );
    }
  }
  &.full {
    &:before {
      clip-path: path(
        evenodd,
        'M0,52 A52,52 0,1,1 104,52 A52,52 0,1,1 0, 52 z m52,20 A20,20 0,1,1 52,32 20,20 0,1,1 52,72 z'
      );
    }
  }
}

.More {
  position: absolute;
  left: -5.5px;
  top: 27px;
  width: 42px;
  height: 17px;
  clip-path: path(
    evenodd,
    'M7.96503 8C7.96503 3.58172 11.5467 0 15.965 0H26.035C30.4533 0 34.035 3.58172 34.035 8V20 H7.96503V8Z'
  );
  transform: scale(0.8);
  backdrop-filter: var(--blur-app-bg);
  background: var(--color-app-bg);
  z-index: -2;
  pointer-events: all;

  &:after {
    content: '...';
    font-size: 15px;
    display: block;
    text-align: center;
    z-index: 10000;
    margin-top: -10px;
    opacity: 0.3;
  }
}

.icon-container {
  display: inline-flex;
  background: none;
  padding: 0;
  border: none;
  opacity: 30%;
  pointer-events: all;
}

.toggledOn {
  opacity: unset;
}

.inactive {
  pointer-events: none;
  opacity: 10%;
}

.recording-overridden {
  opacity: 100%;
  color: red;
}

/**
  * The following styles are used to position the icons in a circular pattern. The slots are named slot1 to slot8 and
  * are positioned using absolute positioning. The slots are positioned in a circle with slot1 at the top and the rest
  * of the slots are positioned in a clockwise direction.
  * ```
  *           slot1
  *      slot8     slot2
  * slot7               slot3
  *      slot6     slot4
  *           slot5
  * ```
 */
.slot1 {
  position: absolute;
  left: 44px;
  top: 8px;
}

.slot2 {
  position: absolute;
  top: 18.54px;
  left: 69.46px;
}

.slot3 {
  position: absolute;
  top: 44px;
  left: 80px;
}

.slot4 {
  position: absolute;
  top: 69.46px;
  left: 69.46px;
}

.slot5 {
  position: absolute;
  left: 44px;
  top: 80px;
}

.below-slot5 {
  position: absolute;
  top: calc(108px - 36px);
  pointer-events: all;
}

.slot6 {
  position: absolute;
  top: 69.46px;
  left: 18.54px;
}

.slot7 {
  position: absolute;
  top: 44px;
  left: 8px;
}

.slot8 {
  position: absolute;
  top: 18.54px;
  left: 18.54px;
}
</style>
