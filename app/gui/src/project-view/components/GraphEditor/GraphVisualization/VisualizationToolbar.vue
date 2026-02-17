<script setup lang="ts">
import { TypeInfo } from '$/providers/openedProjects/project/computedValueRegistry'
import ActionButton from '@/components/ActionButton.vue'
import ComponentTypeLabel from '@/components/ComponentBrowser/ComponentTypeLabel.vue'
import { useVisualizationSelector } from '@/components/GraphEditor/GraphVisualization/visualizationSelector'
import SelectionDropdown from '@/components/SelectionDropdown.vue'
import SelectionDropdownText from '@/components/SelectionDropdownText.vue'
import SvgButton from '@/components/SvgButton.vue'
import type { ToolbarItem } from '@/components/visualizations/toolbar'
import {
  isActionButton,
  isSelectionMenu,
  isTextSelectionMenu,
  isToggleButton,
} from '@/components/visualizations/toolbar'
import { ProjectPath } from '@/util/projectPath'
import { qnLastSegment } from '@/util/qualifiedName'
import { toRef, toValue } from 'vue'
import type { VisualizationIdentifier } from 'ydoc-shared/yjsModel'

const currentVis = defineModel<VisualizationIdentifier>('currentVis', { required: true })

const UNKNOWN_TYPE = 'Unknown'

const props = defineProps<{
  showControls: boolean
  isFocused: boolean
  allVisualizations: ReadonlyArray<VisualizationIdentifier>
  visualizationDefinedToolbar: ReadonlyArray<Readonly<ToolbarItem>> | undefined
  typename: ProjectPath | undefined
  typeinfo: TypeInfo | undefined
}>()

const visualizationSelector = useVisualizationSelector({
  selectedType: currentVis,
  types: toRef(props, 'allVisualizations'),
})
</script>

<template>
  <div class="VisualizationToolbar">
    <template v-if="showControls">
      <div class="toolbarSection">
        <ActionButton action="component.toggleVisualization" />
      </div>
      <div class="toolbarSection">
        <ActionButton action="panel.fullscreen" />
        <SelectionDropdown v-bind="visualizationSelector" :alwaysShowArrow="isFocused" />
      </div>
      <div v-if="visualizationDefinedToolbar" class="visualization-defined-toolbars toolbarSection">
        <template v-for="(item, index) in visualizationDefinedToolbar" :key="index">
          <SvgButton
            v-if="isActionButton(item)"
            :modelValue="undefined"
            :name="item.icon"
            :title="item.title"
            :disabled="item.disabled != null ? toValue(item.disabled) : false"
            :data-testid="item.dataTestid"
            @activate="item.onClick"
          />
          <SvgButton
            v-else-if="isToggleButton(item)"
            v-model="item.toggle.value"
            :name="item.icon"
            :title="item.title"
            :disabled="item.disabled != null ? toValue(item.disabled) : false"
            :data-testid="item.dataTestid"
          />
          <SelectionDropdown
            v-else-if="isSelectionMenu(item)"
            v-model="item.selected.value"
            :options="item.options"
            :title="item.title"
            :alwaysShowArrow="isFocused"
          />
          <SelectionDropdownText
            v-else-if="isTextSelectionMenu(item)"
            v-model="item.selectedTextOption.value"
            :options="item.options"
            :title="item.title"
            :heading="item.heading"
            :alwaysShowArrow="isFocused"
          />
          <div v-else>?</div>
        </template>
      </div>
    </template>
    <div class="after-toolbars node-type" data-testid="visualisationNodeType">
      <ComponentTypeLabel
        v-if="props.typeinfo"
        :unknownLabel="UNKNOWN_TYPE"
        :typeInfo="props.typeinfo"
        testId="visualizationNodeTypeLabel"
      />
      <span v-else>{{
        props.typename?.path ? qnLastSegment(props.typename.path) : UNKNOWN_TYPE
      }}</span>
    </div>
  </div>
</template>

<style scoped>
.VisualizationToolbar {
  flex: 0;
  transition-duration: 100ms;
  transition-property: padding-left;
  width: 100%;
  user-select: none;
  margin-top: 4px;
  display: flex;
  gap: 4px;
}

.after-toolbars {
  margin-left: auto;
  margin-right: 8px;
  overflow: hidden;
  display: flex;
  align-items: center;
}

.node-type {
  font-weight: bold;
}

.toolbarSection {
  display: flex;
  gap: 12px;
  padding: 8px;
  border-radius: var(--radius-full);
  background: var(--color-app-bg);
  backdrop-filter: var(--blur-app-bg);
  &:not(:has(> *)) {
    display: none;
  }
}
</style>
