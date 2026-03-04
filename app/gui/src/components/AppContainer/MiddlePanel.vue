<script setup lang="ts">
import { panelKey, tabFromRoute, useContainerData, type Tab } from '$/providers/container'
import { useOpenedProjects, type Project } from '$/providers/openedProjects'
import GrowingSpinner from '@/components/shared/GrowingSpinner.vue'
import type { SpinnerPhase } from '@/components/shared/LoadingSpinner.vue'
import type { Icon } from '@/util/iconMetadata/iconName'
import { computed, onUnmounted, ref, toRefs, useTemplateRef, watch } from 'vue'
import SelectableTab from './SelectableTab.vue'

const { isCurrentTab, currentTab, tabList, closeTab, focusedPanel, setFocusedPanel } =
  toRefs(useContainerData())
const openedProjects = useOpenedProjects()

const root = useTemplateRef('root')

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

const focusedInBrowser = ref(false)

watch(focusedInBrowser, (isFocused) => {
  if (isFocused) setFocusedPanel.value(currentTab.value)
})

watch(currentTab, (currentTab) => {
  setFocusedPanel.value(currentTab)
  if (!focusedInBrowser.value && currentTab != null) {
    root.value?.focus()
  }
})

const cssClass = computed(() => ({
  focusedPanel: isCurrentTab.value(focusedPanel.value),
}))

onUnmounted(() => {
  if (isCurrentTab.value(focusedPanel.value)) {
    setFocusedPanel.value(null)
  }
})
</script>

<template>
  <div
    ref="root"
    class="MiddlePanel"
    :class="cssClass"
    tabindex="-1"
    @focusin="focusedInBrowser = true"
    @focusout="focusedInBrowser = false"
  >
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
  /* Middle Panel should first give up place when user is shrinking the window. */
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
