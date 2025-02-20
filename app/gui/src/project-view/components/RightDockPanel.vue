<script setup lang="ts">
import ComponentDocumentation from '@/components/ComponentDocumentation.vue'
import DockPanel from '@/components/DockPanel.vue'
import DocumentationEditor from '@/components/DocumentationEditor.vue'
import FunctionSignatureEditor from '@/components/FunctionSignatureEditor.vue'
import { tabButtons, useRightDock } from '@/stores/rightDock'
import { ref } from 'vue'
import { SuggestionId } from 'ydoc-shared/languageServerTypes/suggestions'

const dockStore = useRightDock()

const displayedDocsSuggestion = defineModel<SuggestionId | undefined>('displayedDocs')

const props = defineProps<{ aiMode: boolean }>()

const isFullscreen = ref(false)
</script>

<template>
  <DockPanel
    v-model:size="dockStore.width"
    :show="dockStore.visible"
    :tab="dockStore.displayedTab"
    :tabButtons="tabButtons"
    :contentFullscreen="isFullscreen"
    @update:show="dockStore.setVisible"
    @update:tab="dockStore.switchToTab"
  >
    <template #tab-docs>
      <DocumentationEditor
        v-if="dockStore.markdownDocs"
        ref="docEditor"
        :yText="dockStore.markdownDocs"
        @update:fullscreen="isFullscreen = $event"
      >
        <template #belowToolbar>
          <FunctionSignatureEditor
            v-if="dockStore.inspectedAst"
            :functionAst="dockStore.inspectedAst"
            :methodPointer="dockStore.inspectedMethodPointer"
            :markdownDocs="dockStore.markdownDocs"
          />
        </template>
      </DocumentationEditor>
    </template>
    <template #tab-help>
      <ComponentDocumentation v-model="displayedDocsSuggestion" :aiMode="props.aiMode" />
    </template>
  </DockPanel>
</template>
