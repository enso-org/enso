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
