<script setup lang="ts">
import WithCurrentProject from '$/components/WithCurrentProject.vue'
import { injectRightPanelData, type RightPanelTabId } from '$/providers/rightPanel'
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

const data = injectRightPanelData()

const component = computed(() => {
  return data.displayedTab && data.allTabs.get(data.displayedTab)?.component
})

function tabTooltip(title: ToValue<string>, enabled: ToValue<Result<void>>) {
  const enabledVal = toValue(enabled)
  const titleVal = toValue(title)
  return enabledVal.ok ? titleVal : `${titleVal} - ${enabledVal.error.message('')}`
}

function tabEnabled(id: RightPanelTabId, enabled: ToValue<Result<void>>) {
  const enabledVal = toValue(enabled)
  return data.displayedTab === id || (enabledVal.ok && enabledVal.value)
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
  <div class="RightPanel bg-dashboard" data-testid="right-panel">
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
    <div class="rightBar">
      <div class="shadow" />
      <div class="tabs">
        <SelectableTab
          v-for="[id, tabInfo] in data.allTabs.entries()"
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
  min-width: 0;
  display: flex;
  justify-content: stretch;
  width: 400px;
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
