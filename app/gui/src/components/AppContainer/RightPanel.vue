<script setup lang="ts">
import {
  AssetProperties,
  AssetVersions,
  ProjectExecutionsCalendar,
  ProjectSessions,
} from '$/components/AppContainer/reactTabs'
import SelectableTab from '$/components/AppContainer/SelectableTab.vue'
import WithCurrentProject from '$/components/WithCurrentProject.vue'
import { useRightPanelData, type RightPanelTabId } from '$/providers/rightPanel'
import ComponentHelpPanel from '@/components/ComponentHelpPanel.vue'
import DescriptionEditor from '@/components/DescriptionEditor.vue'
import DocumentationEditor from '@/components/DocumentationEditor.vue'
import ResizeHandles from '@/components/ResizeHandles.vue'
import SizeTransition from '@/components/SizeTransition.vue'
import WithFullscreenMode from '@/components/WithFullscreenMode.vue'
import { useResizeObserver } from '@/composables/events'
import { Rect } from '@/util/data/rect'
import type { Result } from '@/util/data/result'
import { Vec2 } from '@/util/data/vec2'
import type { ToValue } from '@/util/reactivity'
import { computed, toValue, useTemplateRef } from 'vue'

const data = useRightPanelData()

// Not a  part of RightPanelTabInfo, because it would create cyclic imports.
const component = computed(() => {
  switch (data.displayedTab) {
    case 'description':
      return DescriptionEditor
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

const visibleTabs = computed(() => [...data.allTabs.entries()])

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
const style = computed(() =>
  data.width != null ?
    {
      width: `${data.width}px`,
    }
  : undefined,
)
const size = useResizeObserver(contentElement)
const bounds = computed(() => new Rect(Vec2.Zero, size.value))
</script>

<template>
  <div class="RightPanel bg-dashboard" data-testid="right-panel">
    <SizeTransition width :duration="250">
      <div v-if="component != null" ref="contentElement" class="content" :style="style">
        <WithFullscreenMode v-model="data.fullscreen">
          <WithCurrentProject :id="data.focusedProject">
            <div class="contentInner">
              <component :is="component" />
            </div>
          </WithCurrentProject>
        </WithFullscreenMode>
        <ResizeHandles left :modelValue="bounds" @update:modelValue="data.width = $event.width" />
      </div>
    </SizeTransition>
    <div class="rightBar">
      <div class="shadow" />
      <div class="tabs">
        <SelectableTab
          v-for="[id, tabInfo] in visibleTabs"
          :key="id"
          selectionLayoutId="right-tab-highlight"
          :icon="tabInfo.icon"
          :tooltip="tabTooltip(tabInfo.title, tabInfo.enabled)"
          orientation="vertical"
          :selected="data.displayedTab === id"
          :enabled="tabEnabled(id, tabInfo.enabled)"
          @update:selected="data.setTab($event ? id : undefined)"
        />
      </div>
      <div class="filler" />
    </div>
  </div>
</template>

<style lang="css" scoped>
.RightPanel {
  --tab-highlight: rgb(254, 253, 252);
  display: flex;
  position: relative;
  flex-direction: row;
}

.content {
  background-color: rgb(254, 253, 252);
  display: flex;
  justify-content: stretch;
  min-width: 312px;
  width: 400px;
  padding: 1.25rem 1rem;
  overflow: auto;
}

/* React panels rely on being inside columned flex. */
.contentInner {
  width: 100%;
  height: 100%;
  display: flex;
  flex-direction: column;
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
  --selection-color: var(--color-background-hex);
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
