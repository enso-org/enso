<script setup lang="ts">
import SvgIcon from '@/components/SvgIcon.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'

const props = defineProps<{
  isOutputContextEnabledGlobally: boolean
  isOutputContextOverridden: boolean
  isDocsVisible: boolean
  isVisualizationVisible: boolean
  isFullMenuVisible: boolean
}>()
const emit = defineEmits<{
  'update:isOutputContextOverridden': [isOutputContextOverridden: boolean]
  'update:isDocsVisible': [isDocsVisible: boolean]
  'update:isVisualizationVisible': [isVisualizationVisible: boolean]
  startEditing: []
  startEditingComment: []
}>()
</script>

<template>
  <div
    :class="`${props.isFullMenuVisible ? 'CircularMenu full' : 'CircularMenu partial'}`"
    @pointerdown.stop
    @pointerup.stop
    @click.stop
  >
    <SvgIcon
      name="comment"
      class="icon-container button slot2"
      @click.stop="emit('startEditingComment')"
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
      :icon="props.isOutputContextEnabledGlobally ? 'no_auto_replay' : 'auto_replay'"
      class="icon-container button slot7"
      :class="{ 'output-context-overridden': props.isOutputContextOverridden }"
      :alt="`${
        props.isOutputContextEnabledGlobally != props.isOutputContextOverridden ?
          'Disable'
        : 'Enable'
      } output context`"
      :modelValue="props.isOutputContextOverridden"
      @update:modelValue="emit('update:isOutputContextOverridden', $event)"
    />
  </div>
</template>

<style scoped>
.CircularMenu {
  user-select: none;
  position: absolute;
  left: -36px;
  top: -36px;
  width: 114px;
  height: 114px;
  pointer-events: none;

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
}

.toggledOn {
  opacity: unset;
}

.inactive {
  pointer-events: none;
  opacity: 10%;
}

.output-context-overridden {
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

.slot6 {
  position: absolute;
  top: 69.46px;
  left: 18.54px;
}

.slot7 {
  position: absolute;
  top: 44px;
  left: 9px;
}

.slot8 {
  position: absolute;
  top: 18.54px;
  left: 18.54px;
}
</style>
