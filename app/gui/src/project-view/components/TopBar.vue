<script setup lang="ts">
import ActionButton from '@/components/ActionButton.vue'
import ControlGroup from '@/components/ControlGroup.vue'
import ExtendedMenu from '@/components/ExtendedMenu.vue'
import NavBreadcrumbs from '@/components/NavBreadcrumbs.vue'
import SelectionMenu from '@/components/SelectionMenu.vue'
import ZoomControl from '@/components/ZoomControl.vue'
import { useResizeObserver } from '@/composables/events'
import type { DisplayableActionName } from '@/providers/action'
import { useGraphSelection } from '@/providers/graphSelection'
import { computed, useTemplateRef } from 'vue'

const GAP = 8

const projectNameEdited = defineModel<boolean>('projectNameEdited', { default: false })
const props = defineProps<{ zoomLevel: number; menuActions: DisplayableActionName[] }>()
const selection = useGraphSelection()

const rootElement = useTemplateRef('rootElement')
const leftGroups = useTemplateRef('leftGroups')
const zoomControls = useTemplateRef('zoomControls')

const rootSize = useResizeObserver(rootElement)
const leftGroupsSize = useResizeObserver(leftGroups)
const zoomControlsSize = useResizeObserver(zoomControls)

const zoomControlsHidden = computed(
  () => leftGroupsSize.value.x + zoomControlsSize.value.x + 2 * GAP > rootSize.value.x,
)

const extendedMenuZoomControls = computed(() =>
  zoomControlsHidden.value ? { zoomLevel: props.zoomLevel } : undefined,
)

const style = computed(() => ({
  flexWrap: zoomControlsHidden.value ? ('wrap' as const) : ('nowrap' as const),
  '--shrink': zoomControlsHidden.value ? '1' : '0',
  '--gap': `${GAP}px`,
}))
</script>

<template>
  <div ref="rootElement" class="TopBar" :style="style">
    <div class="responsive">
      <div ref="leftGroups" class="alwaysVisibleElements">
        <ExtendedMenu :actions="menuActions" :zoomControls="extendedMenuZoomControls" />
        <NavBreadcrumbs v-model:projectNameEdited="projectNameEdited" />
      </div>

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
        <ActionButton
          action="graph.addComponent"
          label="Input"
          data-testid="add-component-button"
        />
      </ControlGroup>
    </div>

    <div class="invisible flex-1"></div>
    <div ref="zoomControls" class="flex">
      <ZoomControl :zoomLevel="props.zoomLevel" />
    </div>
  </div>
</template>

<style scoped>
.TopBar {
  position: absolute;
  display: flex;
  gap: var(--gap);
  top: 1.25rem;
  left: 0;
  right: 0;
  margin-top: -3px;
  margin-left: 13px;
  margin-right: 13px;
  pointer-events: none;
  align-items: flex-start;
  height: 32px;
  overflow: hidden;

  > * {
    pointer-events: all;
    min-height: 32px;
  }
}

.responsive {
  display: flex;
  gap: var(--gap);
  height: 32px;
  flex-wrap: wrap;
  overflow: hidden;
  flex-shrink: 1;
  min-width: 0;
}

.alwaysVisibleElements {
  display: flex;
  gap: var(--gap);
  pointer-events: none;
  flex-shrink: var(--shrink);
  min-width: 0;

  > * {
    pointer-events: all;
    min-height: 32px;
  }
}

.redButton:active {
  color: #ba4c40;
}
</style>
