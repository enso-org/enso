<script setup lang="ts">
import { panelKey, useContainerData, type Tab } from '$/providers/container'
import { useOpenedProjects, type Project } from '$/providers/openedProjects'
import GrowingSpinner from '@/components/shared/GrowingSpinner.vue'
import type { SpinnerPhase } from '@/components/shared/LoadingSpinner.vue'
import type { Icon } from '@/util/iconMetadata/iconName'
import { computed, toRefs } from 'vue'
import SelectableTab from './SelectableTab.vue'

const { isCurrentTab, currentTab, tabList, closeTab } = toRefs(useContainerData())
const openedProjects = useOpenedProjects()

type TabViewInfo = Tab & {
  dataTestId?: string
  icon: Icon | undefined
  label: string
  loadingPhase?: SpinnerPhase | undefined
}

const tabsViewInfos = computed(() =>
  tabList.value.flatMap((tab): TabViewInfo[] => {
    switch (tab.type) {
      case 'project': {
        const project = openedProjects.get(tab.id)
        if (!project) return []
        const showSpinner =
          project.nextTask?.process === 'opening' || project.nextTask?.process === 'restoring'
        return [
          {
            ...tab,
            dataTestId: 'project-view-tab-button',
            icon: projectIcon(project),
            label:
              project.state.status === 'initialized' ?
                project.state.name.value
              : project.state.info.title,
            loadingPhase:
              showSpinner ?
                project.state.info.mode === 'cloud' ?
                  'loading-slow'
                : 'loading-fast'
              : undefined,
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
</script>

<template>
  <div class="TabBar">
    <div class="tabs" role="tablist">
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
  </div>
</template>

<style scoped>
.TabBar {
  overflow: auto hidden;
  padding-left: 24px;
  scrollbar-width: thin;
  scrollbar-color: rgb(180 180 180) var(--color-dashboard-background);
}
.tabs {
  display: flex;
  flex-direction: row;
}
</style>
