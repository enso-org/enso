<script setup lang="ts">
import {
  AssetProperties,
  AssetVersions,
  ProjectExecutionsCalendar,
  ProjectSessions,
} from '$/components/AppContainer/reactTabs'
import SelectableTab from '$/components/AppContainer/SelectableTab.vue'
import { useContainerData } from '$/providers/container'
import { useRightPanelData, type RightPanelTabId } from '$/providers/rightPanel'
import { optPx } from '$/utils/dom'
import type { ToValue } from '$/utils/reactivity'
import AssetContentsEditor from '@/components/AssetContentsEditor.vue'
import ComponentHelpPanel from '@/components/ComponentHelpPanel.vue'
import DescriptionEditor from '@/components/DescriptionEditor.vue'
import DocumentationEditor from '@/components/DocumentationEditor'
import { useResizeHandles } from '@/components/resizeHandles'
import ResizeHandles from '@/components/ResizeHandles.vue'
import SizeTransition from '@/components/SizeTransition.vue'
import { useResizeObserver } from '@/composables/events'
import { registerHandlers } from '@/providers/action'
import type { Result } from 'enso-common/src/utilities/data/result'
import { computed, toRef, toValue, useTemplateRef } from 'vue'
import PanelContents from './PanelContents.vue'

const width = toRef(useContainerData(), 'rightPanelWidth')
const data = useRightPanelData()
const PANEL_BACKGROUND_COLOR = 'white'
// Inner container inside `PanelContents` component needs separate style, because it's teleported.
const CONTAINER_STYLE = { backgroundColor: PANEL_BACKGROUND_COLOR }

// Not a part of RightPanelTabInfo, because it would create cyclic imports.
const component = computed(() => {
  switch (data.displayedTab) {
    case 'description':
      return DescriptionEditor
    case 'contents':
      return AssetContentsEditor
    case 'settings':
      return AssetProperties
    case 'versions':
      return AssetVersions
    case 'sessions':
      return ProjectSessions
    case 'executionsCalendar':
      return ProjectExecutionsCalendar
    case 'documentation':
      return DocumentationEditor
    case 'help':
      return ComponentHelpPanel
    default:
      return undefined
  }
})

const visibleTabs = computed(() =>
  [...data.allTabs.entries()].filter(([, tabInfo]) => !toValue(tabInfo.hidden)),
)

function tabTooltip(title: ToValue<string>, enabled: ToValue<Result<void>>) {
  const enabledVal = toValue(enabled)
  const titleVal = toValue(title)
  return enabledVal.ok ? titleVal : `${titleVal} - ${enabledVal.error.message('')}`
}

function tabEnabled(id: RightPanelTabId, enabled: ToValue<Result<void>>) {
  const enabledVal = toValue(enabled)
  return data.displayedTab === id || enabledVal.ok
}

const contentElement = useTemplateRef('contentElement')
const resizeHandles = useResizeHandles({
  size: useResizeObserver(contentElement),
})
resizeHandles.onResizeWidth((value) => (width.value = value))
const widthStyle = computed(() => ({ width: optPx(width.value) }))

registerHandlers({
  'panel.close': {
    action: () => data.setTab(undefined),
    available: () => data.tab != null,
  },
})
</script>

<template>
  <div class="RightPanel withBackgroundColor bg-dashboard" data-testid="right-panel">
    <SizeTransition width :duration="250">
      <div v-if="component != null" class="sizeWrapper">
        <div ref="contentElement" class="content" :style="widthStyle">
          <PanelContents
            v-slot="{ toolbarElement }"
            v-model:fullscreen="data.fullscreen"
            :containerStyle="CONTAINER_STYLE"
          >
            <component :is="component" :toolbar="toolbarElement" />
          </PanelContents>
          <ResizeHandles left v-on="resizeHandles.events" />
        </div>
      </div>
    </SizeTransition>
    <div class="rightBar">
      <div class="shadow" />
      <div class="tabs">
        <SelectableTab
          v-for="[id, tabInfo] in visibleTabs"
          :key="id"
          :icon="tabInfo.icon"
          :tooltip="tabTooltip(tabInfo.title, tabInfo.enabled)"
          orientation="vertical"
          :selected="data.displayedTab === id"
          :enabled="tabEnabled(id, tabInfo.enabled)"
          @update:selected="data.setTab($event ? id : undefined)"
        />
      </div>
    </div>
  </div>
</template>

<style lang="css" scoped>
.RightPanel {
  --tab-highlight: white;
  display: flex;
  position: relative;
  flex-direction: row;
  height: 100%;
  z-index: 2;
  --panel-background: v-bind(PANEL_BACKGROUND_COLOR);
}

.content {
  display: flex;
  justify-content: stretch;
  /* Default width, overriden by style tag */
  width: 400px;
  min-width: 312px;
  max-width: calc(40vw - 48px);
  height: 100%;
}

/* This element's visible width will be overwritten by the size transition, but the inner content's
 * will not, preventing content reflow. Content reflow is disruptive to the appearance of the transition, and can affect
 * the framerate drastically.
 */
.sizeWrapper {
  height: 100%;
  min-width: 0;
  flex-grow: 1;
  background-color: var(--panel-background);
}

.rightBar {
  display: flex;
  flex-direction: column;
}

.tabs {
  display: flex;
  flex-direction: column;
  isolation: isolate;
  padding: 16px 0;
}

.SelectableTab {
  --selection-color: var(--panel-background);
  --border-radius: 1rem;
}

.shadow {
  position: absolute;
  width: 100%;
  height: 100%;
  box-shadow:
    0.5px 2.2px 0px rgb(0 0 0 / 0.84%),
    0 1.2px 5.65px 0px rgb(0 0 0 / 1.21%),
    0 2.25px 10.64px 0 rgb(0 0 0 / 1.5%),
    0 4px 19px 0 rgb(0 0 0 / 1.79%),
    0 7.5px 35.5px 0 rgb(0 0 0 / 2.16%),
    0 18px 85px 0 rgb(0 0 0 / 3%);
  clip-path: polygon(-100vw 0, 100% 0, 100% 100%, -100vw 100%);
  z-index: -1;
}
</style>
