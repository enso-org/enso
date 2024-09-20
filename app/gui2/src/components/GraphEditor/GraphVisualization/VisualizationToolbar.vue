<script setup lang="ts">
import FullscreenButton from '@/components/FullscreenButton.vue'
import SelectionDropdown from '@/components/SelectionDropdown.vue'
import SvgButton from '@/components/SvgButton.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import {
  isActionButton,
  isSelectionMenu,
  isToggleButton,
  ToolbarItem,
} from '@/components/visualizations/toolbar'
import VisualizationSelector from '@/components/VisualizationSelector.vue'
import { useEvent } from '@/composables/events'
import { provideInteractionHandler } from '@/providers/interactionHandler'
import { isQualifiedName, qnLastSegment } from '@/util/qualifiedName'
import { computed, toValue } from 'vue'
import { VisualizationIdentifier } from 'ydoc-shared/yjsModel'

const isFullscreen = defineModel<boolean>('isFullscreen', { required: true })
const currentVis = defineModel<VisualizationIdentifier>('currentVis', { required: true })

const props = defineProps<{
  showControls: boolean
  hideVisualizationButton: 'show' | 'hide' | 'invisible'
  isFullscreenAllowed: boolean
  allTypes: Iterable<VisualizationIdentifier>
  visualizationDefinedToolbar: Readonly<ToolbarItem[]> | undefined
  typename: string | undefined
}>()

const emit = defineEmits<{
  hide: []
}>()

const UNKNOWN_TYPE = 'Unknown'
const nodeShortType = computed(() =>
  props.typename != null && isQualifiedName(props.typename) ?
    qnLastSegment(props.typename)
  : UNKNOWN_TYPE,
)

const interaction = provideInteractionHandler()
useEvent(window, 'pointerdown', (e) => interaction.handlePointerEvent(e, 'pointerdown'), {
  capture: true,
})
useEvent(window, 'pointerup', (e) => interaction.handlePointerEvent(e, 'pointerup'), {
  capture: true,
})
</script>

<template>
  <div class="VisualizationToolbar">
    <template v-if="showControls">
      <div
        v-if="hideVisualizationButton !== 'hide'"
        class="toolbar"
        :class="{ invisible: hideVisualizationButton === 'invisible' }"
      >
        <SvgButton name="eye" title="Hide visualization" @click.stop="emit('hide')" />
      </div>
      <div class="toolbar">
        <FullscreenButton v-if="isFullscreenAllowed" v-model="isFullscreen" />
        <VisualizationSelector v-model="currentVis" :types="allTypes" />
      </div>
      <div v-if="visualizationDefinedToolbar" class="visualization-defined-toolbars">
        <div class="toolbar">
          <template v-for="(item, index) in visualizationDefinedToolbar" :key="index">
            <SvgButton
              v-if="isActionButton(item)"
              :name="item.icon"
              :title="item.title"
              :onClick="item.onClick"
              :disabled="item.disabled != null ? toValue(item.disabled) : false"
              :data-testid="item.dataTestid"
            />
            <ToggleIcon
              v-else-if="isToggleButton(item)"
              v-model="item.toggle.value"
              :icon="item.icon"
              :title="item.title"
              :disabled="item.disabled != null ? toValue(item.disabled) : false"
              :data-testid="item.dataTestid"
            />
            <SelectionDropdown
              v-else-if="isSelectionMenu(item)"
              v-model="item.selected.value"
              :options="item.options"
              :title="item.title"
              alwaysShowArrow
            />
            <div v-else>?</div>
          </template>
        </div>
      </div>
    </template>
    <div
      class="after-toolbars node-type"
      :title="props.typename ?? UNKNOWN_TYPE"
      v-text="nodeShortType"
    />
  </div>
</template>

<style scoped>
.VisualizationToolbar {
  flex: 0;
  transition-duration: 100ms;
  transition-property: padding-left;
  width: 100%;
  user-select: none;
  margin-top: 4px;
  display: flex;
  gap: 4px;
}

.after-toolbars {
  display: flex;
  flex-direction: row;
  justify-content: flex-end;
  margin-left: auto;
  margin-right: 8px;
  overflow: hidden;
  width: calc(var(--node-size-x) - var(--permanent-toolbar-width));
}

.node-type {
  font-weight: bold;
}

.toolbar {
  position: relative;
  display: flex;
  border-radius: var(--radius-full);
  gap: 12px;
  padding: 8px;
  z-index: 20;

  &:before {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    z-index: -1;
    border-radius: var(--radius-full);
    background: var(--color-app-bg);
    backdrop-filter: var(--blur-app-bg);
  }
}

.toolbar:not(:first-child):not(:has(> *)) {
  display: none;
}

.toolbar > :deep(*) {
  position: relative;
}
</style>
