<script setup lang="ts">
import NavBreadcrumbs from '@/components/NavBreadcrumbs.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { injectStackNavigator } from '@/providers/graphStackNavigator'
import SvgButton from './SvgButton.vue'

const stackNavigator = injectStackNavigator()
</script>

<template>
  <div class="NavBar">
    <SvgIcon name="graph_editor" draggable="false" />
    <div class="breadcrumbs-controls">
      <SvgButton
        name="arrow_left"
        :disabled="!stackNavigator.allowNavigationLeft"
        title="Back"
        @click.stop="stackNavigator.exitNode"
      />
      <SvgButton
        name="arrow_right"
        :disabled="!stackNavigator.allowNavigationRight"
        title="Forward"
        @click.stop="stackNavigator.enterNextNodeFromHistory"
      />
    </div>
    <NavBreadcrumbs />
  </div>
</template>

<style scoped>
.NavBar {
  user-select: none;
  display: flex;
  border-radius: var(--radius-full);
  background: var(--color-frame-bg);
  backdrop-filter: var(--blur-app-bg);
  place-items: center;
  gap: 12px;
  padding-left: 8px;
  padding-right: 10px;
  padding-top: 4px;
  padding-bottom: 4px;

  > .breadcrumbs-controls {
    display: flex;
  }
}
</style>
