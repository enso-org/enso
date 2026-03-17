<script setup lang="ts">
import { panelKey, tabFromRoute, useContainerData } from '$/providers/container'
import { computed, onUnmounted, ref, toRefs, useTemplateRef, watch } from 'vue'

const { isCurrentTab, currentTab, focusedPanel, setFocusedPanel } = toRefs(useContainerData())

const root = useTemplateRef('root')

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
