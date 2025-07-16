<script setup lang="ts">
import ActionButton from '@/components/ActionButton.vue'
import ControlGroup from '@/components/ControlGroup.vue'
import ExtendedMenu from '@/components/ExtendedMenu.vue'
import NavBreadcrumbs from '@/components/NavBreadcrumbs.vue'
import SelectionMenu from '@/components/SelectionMenu.vue'
import ZoomControl from '@/components/ZoomControl.vue'
import { type DisplayableActionName } from '@/providers/action'
import { injectGraphSelection } from '@/providers/graphSelection'

const projectNameEdited = defineModel<boolean>('projectNameEdited', { default: false })
const props = defineProps<{ zoomLevel: number; menuActions: DisplayableActionName[] }>()
const selection = injectGraphSelection()
</script>

<template>
  <div class="TopBar">
    <ExtendedMenu :actions="menuActions" />
    <NavBreadcrumbs v-model:projectNameEdited="projectNameEdited" />
    <ControlGroup>
      <ActionButton class="redButton" action="graph.refreshExecution" />
      <ActionButton class="redButton" action="graph.recomputeAll" />
    </ControlGroup>
    <ControlGroup>
      <ActionButton action="graph.undo" />
      <ActionButton action="graph.redo" />
    </ControlGroup>
    <SelectionMenu v-if="selection.selected.size > 1" />
    <ControlGroup v-else>
      <ActionButton action="graph.addComponent" label="Input" data-testid="add-component-button" />
    </ControlGroup>

    <div class="invisible flex-1"></div>
    <ZoomControl :zoomLevel="props.zoomLevel" />
  </div>
</template>

<style scoped>
.TopBar {
  position: absolute;
  display: flex;
  gap: 8px;
  top: 8px;
  left: 0;
  right: 0;
  margin-left: 11px;
  margin-right: 11px;
  pointer-events: none;
  align-items: flex-start;
  > * {
    pointer-events: all;
    min-height: 32px;
  }
}

.redButton:active {
  color: #ba4c40;
}
</style>
