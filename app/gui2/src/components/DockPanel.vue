<script setup lang="ts">
import { documentationEditorBindings } from '@/bindings'
import ResizeHandles from '@/components/ResizeHandles.vue'
import SizeTransition from '@/components/SizeTransition.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import { useResizeObserver } from '@/composables/events'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { tabClipPath } from 'enso-common/src/utilities/style/tabBar'
import { computed, ref } from 'vue'

const MIN_DOCK_SIZE_PX = 200
const TAB_EDGE_MARGIN_PX = 4
const TAB_SIZE_PX = { width: 48 - TAB_EDGE_MARGIN_PX, height: 48 }
const TAB_RADIUS_PX = 8

type Tab = 'docs' | 'help'

const show = defineModel<boolean>('show', { required: true })
const size = defineModel<number | undefined>('size')
const tab = defineModel<Tab>('tab')

const slideInPanel = ref<HTMLElement>()

const computedSize = useResizeObserver(slideInPanel)
const computedBounds = computed(() => new Rect(Vec2.Zero, computedSize.value))

function clampSize(size: number) {
  return Math.max(size, MIN_DOCK_SIZE_PX)
}

const style = computed(() => ({
  width: size.value != null ? `${clampSize(size.value)}px` : 'var(--right-dock-default-width)',
}))

const tabStyle = {
  clipPath: tabClipPath(TAB_SIZE_PX, TAB_RADIUS_PX, 'right'),
  width: `${TAB_SIZE_PX.width}px`,
  height: `${TAB_SIZE_PX.height}px`,
  margin: `${-TAB_RADIUS_PX}px ${TAB_EDGE_MARGIN_PX}px ${-TAB_RADIUS_PX}px 0`,
  paddingLeft: `${TAB_EDGE_MARGIN_PX / 2}px`,
}
</script>

<template>
  <ToggleIcon
    v-model="show"
    :title="`Documentation Panel (${documentationEditorBindings.bindings.toggle.humanReadable})`"
    icon="right_panel"
    class="toggleDock"
  />
  <SizeTransition width :duration="100">
    <div v-if="show" ref="slideInPanel" :style="style" class="DockPanel" data-testid="rightDock">
      <div class="content">
        <slot v-if="tab == 'docs'" name="docs" />
        <slot v-else-if="tab == 'help'" name="help" />
      </div>
      <div class="tabBar">
        <div class="tab" :style="tabStyle">
          <ToggleIcon
            :modelValue="tab == 'docs'"
            title="Documentation Editor"
            icon="text"
            @update:modelValue="tab = 'docs'"
          />
        </div>
        <div class="tab" :style="tabStyle">
          <ToggleIcon
            :modelValue="tab == 'help'"
            title="Component Help"
            icon="help"
            @update:modelValue="tab = 'help'"
          />
        </div>
      </div>
      <ResizeHandles left :modelValue="computedBounds" @update:modelValue="size = $event.width" />
    </div>
  </SizeTransition>
</template>

<style scoped>
.DockPanel {
  position: relative;
  --icon-margin: 16px;
  --icon-size: 16px;
  display: flex;
  flex-direction: row;
  justify-content: stretch;
}

.content {
  width: 100%;
  height: 100%;
  background-color: #fff;
  min-width: 0;
}

.tabBar {
  flex: none;
  width: calc(2 * var(--icon-margin) + var(--icon-size));
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 8px;
  padding-top: calc(2 * var(--icon-margin) + var(--icon-size));
}

.tab {
  display: flex;
  align-items: center;
  justify-content: center;
  &:has(.toggledOn) {
    background-color: #fff;
  }
}

.toggleDock {
  --icon-margin: 16px; /* Must match `--icon-margin` defined above, which is not in scope because of the teleport. */
  z-index: 1;
  position: absolute;
  right: var(--icon-margin);
  top: var(--icon-margin);
}
</style>
