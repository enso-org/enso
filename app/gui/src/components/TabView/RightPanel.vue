<script setup lang="ts">
import WithCurrentProject from '$/components/WithCurrentProject.vue'
import { injectConainerData, RightPanelTabId } from '$/providers/container'
import ResizeHandles from '@/components/ResizeHandles.vue'
import SizeTransition from '@/components/SizeTransition.vue'
import WithFullscreenMode from '@/components/WithFullscreenMode.vue'
import { useResizeObserver } from '@/composables/events'
import { Rect } from '@/util/data/rect'
import { Result } from '@/util/data/result'
import { Vec2 } from '@/util/data/vec2'
import { ToValue } from '@/util/reactivity'
import { computed, ref, toValue } from 'vue'
import SelectableTab from './SelectableTab.vue'

const { rightPanel: data } = injectConainerData()

const displayedTab = computed(() => data.temporaryTab ?? data.tab)

const component = computed(() => {
  return displayedTab.value && data.allTabs.get(displayedTab.value)?.component
})

function setTab(tab: RightPanelTabId | undefined) {
  data.tab = tab
  data.setTemporaryTab(undefined)
}

function tabTooltip(title: ToValue<string>, enabled: ToValue<Result<boolean>>) {
  const enabledVal = toValue(enabled)
  const titleVal = toValue(title)
  return enabledVal.ok ? titleVal : `${titleVal} - ${enabledVal.error.message('')}`
}

function tabEnabled(id: RightPanelTabId, enabled: ToValue<Result<boolean>>) {
  const enabledVal = toValue(enabled)
  return displayedTab.value === id || (enabledVal.ok && enabledVal.value)
}

const contentElement = ref<HTMLElement>()
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
  <div class="RightPanel bg-dashboard">
    <SizeTransition width :duration="100">
      <div v-if="component != null" ref="contentElement" class="content" :style="style">
        <WithFullscreenMode :fullscreen="data.fullscreen">
          <WithCurrentProject :id="data.focusedProject">
            <component :is="component" />
          </WithCurrentProject>
        </WithFullscreenMode>
        <ResizeHandles left :modelValue="bounds" @update:modelValue="data.width = $event.width" />
      </div>
    </SizeTransition>
    <div class="tabs">
      <SelectableTab
        v-for="[id, tabInfo] in data.allTabs.entries()"
        :key="id"
        layoutId="right-tab-highlight"
        :icon="tabInfo.icon"
        :tooltip="tabTooltip(tabInfo.title, tabInfo.enabled)"
        orientation="vertical"
        :selected="displayedTab === id"
        :enabled="tabEnabled(id, tabInfo.enabled)"
        @update:selected="setTab($event ? id : undefined)"
      />
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
  min-width: 0;
  display: flex;
  justify-content: stretch;
  width: 400px;
}

.tabs {
  display: flex;
  flex-direction: column;
  padding: 8px 0;
  isolation: isolate;
}
</style>
