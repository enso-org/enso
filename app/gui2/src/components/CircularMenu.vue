<script setup lang="ts">
import { graphBindings } from '@/bindings'
import ColorRing from '@/components/ColorRing.vue'
import SvgButton from '@/components/SvgButton.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import { ref } from 'vue'

const nodeColor = defineModel<string | undefined>('nodeColor')
const props = defineProps<{
  isRecordingEnabledGlobally: boolean
  isRecordingOverridden: boolean
  isVisualizationEnabled: boolean
  isFullMenuVisible: boolean
  isRemovable: boolean
  matchableNodeColors: Set<string>
  documentationUrl: string | undefined
}>()
const emit = defineEmits<{
  'update:isRecordingOverridden': [isRecordingOverridden: boolean]
  'update:isVisualizationEnabled': [isVisualizationEnabled: boolean]
  startEditing: []
  startEditingComment: []
  openFullMenu: []
  delete: []
}>()

const showColorPicker = ref(false)

function openDocs(url: string) {
  window.open(url, '_blank')
}

function readableBinding(binding: keyof (typeof graphBindings)['bindings']) {
  return graphBindings.bindings[binding].humanReadable
}
</script>

<template>
  <div
    class="CircularMenu"
    :class="{
      partial: !props.isFullMenuVisible,
      menu: !showColorPicker,
    }"
  >
    <template v-if="!showColorPicker">
      <template v-if="isFullMenuVisible">
        <SvgButton
          v-if="documentationUrl"
          name="help"
          class="slot1"
          :title="`Open Documentation (${readableBinding('openDocumentation')})`"
          @click.stop="openDocs(documentationUrl)"
        />
        <SvgButton
          name="comment"
          class="slot2"
          title="Comment"
          @click.stop="emit('startEditingComment')"
        />
        <SvgButton
          name="paint_palette"
          class="slot3"
          title="Color"
          @click.stop="showColorPicker = true"
        />
        <SvgButton
          name="trash2"
          class="slot4"
          :title="`Delete (${readableBinding('deleteSelected')})`"
          data-testid="removeNode"
          :disabled="!isRemovable"
          @click.stop="emit('delete')"
        />
      </template>
      <div v-else class="More" @pointerdown.stop="emit('openFullMenu')"></div>
      <ToggleIcon
        icon="eye"
        class="slot5"
        title="Visualization"
        :modelValue="props.isVisualizationEnabled"
        @update:modelValue="emit('update:isVisualizationEnabled', $event)"
      />
      <SvgButton
        name="edit"
        class="slot6"
        title="Code Edit"
        data-testid="edit-button"
        @click.stop="emit('startEditing')"
      />
      <ToggleIcon
        icon="record"
        class="slot7 record"
        data-testid="toggleRecord"
        title="Write Always"
        :modelValue="props.isRecordingOverridden"
        @update:modelValue="emit('update:isRecordingOverridden', $event)"
      />
    </template>
    <ColorRing
      v-else
      v-model="nodeColor"
      :matchableColors="matchableNodeColors"
      :initialColorAngle="90"
      @close="showColorPicker = false"
    />
  </div>
</template>

<style scoped>
.CircularMenu {
  position: absolute;
  left: -36px;
  bottom: -36px;
  width: var(--outer-diameter);
  height: var(--outer-diameter);
  user-select: none;
  pointer-events: none;
  /* This is a variable so that it can be referenced in computations,
     but currently it can't be changed due to many hard-coded values below. */
  --outer-diameter: 104px;
  --full-ring-path: path(
    evenodd,
    'M0,52 A52,52 0,1,1 104,52 A52,52 0,1,1 0, 52 z m52,20 A20,20 0,1,1 52,32 20,20 0,1,1 52,72 z'
  );
}

.menu {
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
    clip-path: var(--full-ring-path);
  }

  &.partial {
    &:before {
      top: 36px;
      clip-path: path(
        'm0 16a52 52 0 0 0 52 52a16 16 0 0 0 0 -32a20 20 0 0 1-20-20a16 16 0 0 0-32 0'
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

:deep(.ColorRing .gradient) {
  clip-path: var(--full-ring-path);
}

.inactive {
  pointer-events: none;
  opacity: 10%;
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
