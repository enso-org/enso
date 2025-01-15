<script setup lang="ts" generic="Tab extends string">
import { documentationEditorBindings } from '@/bindings'
import ResizeHandles from '@/components/ResizeHandles.vue'
import SizeTransition from '@/components/SizeTransition.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import { useResizeObserver } from '@/composables/events'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { TabButton } from '@/util/tabs'
import { tabClipPath } from 'enso-common/src/utilities/style/tabBar'
import { computed, ref } from 'vue'

const TAB_EDGE_MARGIN_PX = 4
const TAB_SIZE_PX = { width: 48 - TAB_EDGE_MARGIN_PX, height: 48 }
const TAB_RADIUS_PX = 8

const show = defineModel<boolean>('show', { required: true })
const size = defineModel<number | undefined>('size')
const currentTab = defineModel<Tab>('tab')

const props = defineProps<{
  contentFullscreen: boolean
  tabButtons: TabButton<Tab>[]
}>()

const slideInPanel = ref<HTMLElement>()

const computedSize = useResizeObserver(slideInPanel)
const computedBounds = computed(() => new Rect(Vec2.Zero, computedSize.value))

const style = computed(() =>
  size.value != null ?
    {
      '--dock-panel-width': `${size.value}px`,
    }
  : undefined,
)

const tabStyle = {
  clipPath: tabClipPath(TAB_SIZE_PX, TAB_RADIUS_PX, 'right'),
  width: `${TAB_SIZE_PX.width}px`,
  height: `${TAB_SIZE_PX.height}px`,
  margin: `${-TAB_RADIUS_PX}px ${TAB_EDGE_MARGIN_PX}px ${-TAB_RADIUS_PX}px 0`,
  paddingLeft: `${TAB_EDGE_MARGIN_PX / 2}px`,
}
</script>

<template>
  <div class="DockPanel" data-testid="rightDockRoot">
    <ToggleIcon
      v-model="show"
      :title="`Documentation Panel (${documentationEditorBindings.bindings.toggle.humanReadable})`"
      icon="right_panel"
      class="toggleDock"
      :class="{ aboveFullscreen: props.contentFullscreen }"
    />
    <SizeTransition width :duration="100">
      <div v-if="show" ref="slideInPanel" :style="style" class="panelOuter" data-testid="rightDock">
        <div class="panelInner">
          <div class="content">
            <slot :name="`tab-${currentTab}`" />
          </div>
          <div class="tabBar">
            <div
              v-for="{ tab, title, icon } in props.tabButtons"
              :key="tab"
              class="tab"
              :style="tabStyle"
            >
              <ToggleIcon
                :modelValue="currentTab == tab"
                :title="title"
                :icon="icon"
                @update:modelValue="currentTab = tab"
              />
            </div>
          </div>
          <ResizeHandles
            left
            :modelValue="computedBounds"
            @update:modelValue="size = $event.width"
          />
        </div>
      </div>
    </SizeTransition>
  </div>
</template>

<style scoped>
.DockPanel {
  display: block;
  --dock-panel-min-width: 258px;
  width: fit-content;
}

/* Outer panel container; this element's visible width will be overwritten by the size transition, but the inner panel's
 * will not, preventing content reflow. Content reflow is disruptive to the appearance of the transition, and can affect
 * the framerate drastically.
 */
.panelOuter {
  min-width: var(--dock-panel-min-width);
  width: var(--dock-panel-width, var(--right-dock-default-width));
  height: 100%;
}

.panelInner {
  min-width: var(--dock-panel-min-width);
  width: var(--dock-panel-width, var(--right-dock-default-width));
  height: 100%;
  position: relative;
  --icon-margin: 16px; /* `--icon-margin` in `.toggleDock` must match this value. */
  --icon-size: 16px;
  display: flex;
  flex-direction: row;
  justify-content: stretch;
}

.content {
  width: 100%;
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
  &.aboveFullscreen {
    z-index: 2;
  }
}
</style>
