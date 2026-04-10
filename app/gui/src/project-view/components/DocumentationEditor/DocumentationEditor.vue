<script setup lang="ts">
import WithCurrentProject from '$/components/WithCurrentProject.vue'
import { useRightPanelData } from '$/providers/rightPanel'
import { providePopoverRoot } from '@/providers/popoverRoot'
import { useTemplateRef, watchEffect } from 'vue'
import ClosedProjectDocumentationEditor from './ClosedProjectDocumentationEditor.vue'
import OpenedProjectDocumentationEditor from './OpenedProjectDocumentationEditor.vue'

const props = defineProps<{ toolbar: HTMLElement | string }>()

watchEffect(() => console.debug('TOOLBAR', props.toolbar))

const rightPanel = useRightPanelData()

const rootElement = useTemplateRef('rootElement')
providePopoverRoot(rootElement)
</script>

<template>
  <div ref="rootElement" class="DocumentationEditor">
    <WithCurrentProject :id="rightPanel.focusedProject">
      <OpenedProjectDocumentationEditor :toolbar="toolbar" />
      <template #fallback>
        <ClosedProjectDocumentationEditor :toolbar="toolbar" />
      </template>
    </WithCurrentProject>
  </div>
</template>

<style scoped>
.DocumentationEditor {
  display: flex;
  flex-direction: column;
  background-color: #fff;
  height: 100%;
  width: 100%;
  padding-left: 4px;
  padding-right: 4px;
}
</style>
