<script setup lang="ts">
import { panelKey, tabFromRoute, useContainerData, type Tab } from '$/providers/container'
import { useOpenedProjects, type Project } from '$/providers/openedProjects'
import GrowingSpinner from '@/components/shared/GrowingSpinner.vue'
import type { SpinnerPhase } from '@/components/shared/LoadingSpinner.vue'
import type { Icon } from '@/util/iconMetadata/iconName'
import { computed, ref, toRefs, watchEffect } from 'vue'
import SelectableTab from './SelectableTab.vue'

const { isCurrentTab, currentTab, tabList, closeTab, focusedPanel, setFocusedPanel } =
  toRefs(useContainerData())
const openedProjects = useOpenedProjects()

type TabViewInfo = Tab & {
  dataTestId?: string
  icon: Icon | undefined
  label: string
  loadingPhase?: SpinnerPhase
}

const tabsViewInfos = computed(() =>
  tabList.value.flatMap((tab): TabViewInfo[] => {
    switch (tab.type) {
      case 'project': {
        const project = openedProjects.get(tab.id)
        if (!project) return []
        return [
          {
            ...tab,
            dataTestId: 'project-view-tab-button',
            icon: projectIcon(project),
            label:
              project.state.status === 'initialized' ?
                project.state.name.value
              : project.state.info.title,
            ...((
              project.nextTask?.process === 'opening' || project.nextTask?.process === 'restoring'
            ) ?
              {
                loadingPhase: project.state.info.mode === 'cloud' ? 'loading-slow' : 'loading-fast',
              }
            : {}),
          },
        ]
      }
      case 'settings': {
        return [
          {
            ...tab,
            icon: 'settings',
            label: 'Settings',
          },
        ]
      }
    }
  }),
)

function projectIcon(project: Project): Icon | undefined {
  if (project.error != null) {
    return 'error'
  }
  if (project.state.status === 'closed-by-backend') {
    return 'warning'
  }
  if (project.nextTask?.process === 'opening' || project.nextTask?.process === 'restoring') {
    return undefined
  }
  return 'graph_editor'
}

const isFocused = ref(false)

watchEffect(() => {
  if ((isFocused.value || focusedPanel.value.type !== 'drive') && currentTab.value != null) {
    setFocusedPanel.value(currentTab.value)
  }
})
</script>

<template>
  <div class="MiddlePanel" tabindex="-1" @focusin="isFocused = true" @focusout="isFocused = false">
    <div class="tablist" role="tablist">
      <SelectableTab
        v-for="tab in tabsViewInfos"
        :key="panelKey(tab)"
        :data-testid="tab.dataTestId"
        :selected="isCurrentTab(tab)"
        :icon="tab.icon"
        :label="tab.label"
        @close="closeTab(tab)"
        @update:selected="$event && (currentTab = tab)"
      >
        <GrowingSpinner v-if="tab.loadingPhase != null" :phase="tab.loadingPhase" :size="16" />
      </SelectableTab>
    </div>
    <RouterView v-slot="{ Component, route }">
      <KeepAlive>
        <component
          :is="Component"
          v-if="Component"
          :key="currentTab && panelKey(currentTab)"
          :tab="tabFromRoute(route)"
        />
      </KeepAlive>
    </RouterView>
  </div>
</template>

<style scoped>
.MiddlePanel {
  display: flex;
  flex-direction: column;
  width: 100%;
  min-width: 0;
  flex-shrink: 1000000;
}
.tablist {
  background-color: rgba(0, 0, 0, 0.1);
  padding: 0 8px;
  display: flex;
  flex-direction: row;
  /* Create a stacking context for tab highlight, so it's under all tabs' contents. */
  isolation: isolate;
  font-family: var(--font-sans);
}
</style>
