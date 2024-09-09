<script setup lang="ts">
/** @file
 * Contains a visualization's toolbars and the visualization itself. The panel's size is determined by its enclosing
 * container (in fullscreen mode, this will be the fullscreen-container element).
 */

import FullscreenButton from '@/components/FullscreenButton.vue'
import SvgButton from '@/components/SvgButton.vue'
import VisualizationSelector from '@/components/VisualizationSelector.vue'
import WithFullscreenMode from '@/components/WithFullscreenMode.vue'
import { isTriggeredByKeyboard } from '@/composables/events'
import { useVisualizationConfig } from '@/providers/visualizationConfig'
import { isQualifiedName, qnLastSegment } from '@/util/qualifiedName'
import { computed, ref } from 'vue'

const props = defineProps<{
  overflow?: boolean
  toolbarOverflow?: boolean
}>()

const config = useVisualizationConfig()

function onWheel(event: WheelEvent) {
  if (
    event.currentTarget instanceof Element &&
    (config.fullscreen ||
      event.currentTarget.scrollWidth > event.currentTarget.clientWidth ||
      event.currentTarget.scrollHeight > event.currentTarget.clientHeight)
  ) {
    event.stopPropagation()
  }
}

const UNKNOWN_TYPE = 'Unknown'
const nodeShortType = computed(() =>
  config.nodeType != null && isQualifiedName(config.nodeType) ?
    qnLastSegment(config.nodeType)
  : UNKNOWN_TYPE,
)

const fullscreenAnimating = ref(false)

const isSelectorVisible = ref(false)

function hideSelector() {
  requestAnimationFrame(() => (isSelectorVisible.value = false))
}
</script>

<template>
  <WithFullscreenMode
    v-model:savedSize="config.savedSize"
    :fullscreen="config.fullscreen"
    @update:animating="fullscreenAnimating = $event"
  >
    <div
      class="VisualizationPanel"
      :class="{
        fullscreen: config.fullscreen || fullscreenAnimating,
        nonInteractive: config.isPreview,
      }"
      :style="{
        '--color-visualization-bg': config.background,
      }"
    >
      <div class="toolbars">
        <div
          v-if="!config.isPreview && !config.fullscreen"
          class="toolbar"
          :class="{ invisible: config.isCircularMenuVisible }"
        >
          <SvgButton name="eye" title="Hide visualization" @click.stop="config.hide()" />
        </div>
        <div v-if="!config.isPreview" class="toolbar">
          <FullscreenButton v-if="config.isFullscreenAllowed" v-model="config.fullscreen" />
          <div class="icon-container">
            <SvgButton
              :name="config.icon ?? 'columns_increasing'"
              title="Visualization Selector"
              @click.stop.prevent="
                (!isSelectorVisible || isTriggeredByKeyboard($event)) &&
                  (isSelectorVisible = !isSelectorVisible)
              "
            />
            <Suspense>
              <VisualizationSelector
                v-if="isSelectorVisible"
                :types="config.types"
                :modelValue="config.currentType"
                @hide="hideSelector"
                @update:modelValue="(isSelectorVisible = false), config.updateType($event)"
              />
            </Suspense>
          </div>
        </div>
        <div
          v-if="$slots.toolbar && !config.isPreview"
          class="visualization-defined-toolbars"
          :class="{ overflow: props.toolbarOverflow }"
        >
          <div class="toolbar"><slot name="toolbar"></slot></div>
        </div>
        <div
          class="after-toolbars node-type"
          :title="config.nodeType ?? UNKNOWN_TYPE"
          v-text="nodeShortType"
        />
      </div>
      <div
        class="content scrollable"
        :class="{ overflow: props.overflow }"
        @wheel.passive="onWheel"
      >
        <slot></slot>
      </div>
    </div>
  </WithFullscreenMode>
</template>

<style scoped>
.VisualizationPanel {
  --permanent-toolbar-width: 240px;
  color: var(--color-text);
  cursor: default;
  position: relative;
  display: flex;
  flex-direction: column;
  height: 100%;
  &.fullscreen {
    background: var(--color-visualization-bg);
  }
}

.toolbars {
  flex: 0;
  transition-duration: 100ms;
  transition-property: padding-left;
}

.content {
  overflow: auto;
  contain: strict;
  height: 100%;
}

.toolbars {
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

.visualization-defined-toolbars {
  max-width: calc(100% - var(--permanent-toolbar-width));
  overflow: hidden;
}

.toolbar > :deep(*) {
  position: relative;
}

.overflow {
  overflow: visible;
}

.nonInteractive {
  pointer-events: none;
}
</style>
