<script setup lang="ts">
import { useBackends } from '$/providers/backends'
import { useRightPanelData } from '$/providers/rightPanel'
import FunctionSignatureEditor from '@/components/FunctionSignatureEditor.vue'
import MarkdownEditor from '@/components/MarkdownEditor.vue'
import { provideDocumentationImages } from '@/components/MarkdownEditor/imageFiles'
import { Err, mapOk, Ok, unwrapOr } from '@/util/data/result'
import { methodPointerEquals } from '@/util/methodPointer'
import { ResultComponent } from '@/util/react'
import { computed, effectScope, onScopeDispose, ref, toRef, watch } from 'vue'
import * as Y from 'yjs'

const rightPanel = useRightPanelData()
const focusedAsset = toRef(rightPanel, 'focusedAsset')
const { backendForType } = useBackends()
const backendForAsset = computed(() => {
  if (rightPanel.context?.category == null) return null
  return backendForType(rightPanel.context.category.backend)
})

const content = ref<Y.Text>()

watch(() => rightPanel.focusedAsset, (newAsset, _, onCleanup) => {
  if (newAsset != undefined) {
    const description = new Y.Text(newAsset.description)

    const watchers = effectScope()
    onScopeDispose(() => watchers.stop())
    watchers.run(() => {
      watch(() => newAsset.description, (newDescription) => {
        if (newDescription != )
      })
    })
  }
})

const currentMethodAst = computed(() => openedProject.value?.graph.currentMethod.ast)

const currentMethodPointer = computed(
  () => openedProject.value && unwrapOr(openedProject.value.graph.currentMethod.pointer, undefined),
)
const displaySignatureEditor = computed(
  () =>
    currentMethodPointer.value &&
    openedProject.value?.store.entryPoint &&
    !methodPointerEquals(currentMethodPointer.value, openedProject.value.store.entryPoint),
)

const editorMarkdown = computed(() => {
  if (currentMethodAst.value != null) {
    return mapOk(currentMethodAst.value, (ast) => ast.mutableDocumentationMarkdown())
  } else if (rightPanel.focusedAsset) {
    return Ok(rightPanel.focusedAsset.description ?? '')
  } else {
    return Err('No documentation available')
  }
})

provideDocumentationImages({
  openedProject,
  backend: backendForAsset,
  projectId,
})
</script>

<template>
  <div class="DocumentationEditor">
    <MarkdownEditor
      v-if="editorMarkdown.ok"
      :content="editorMarkdown.value"
      contentTestId="documentation-editor-content"
    >
      <template #belowToolbar>
        <FunctionSignatureEditor
          v-if="displaySignatureEditor && currentMethodAst?.ok && openedProject"
          :projectId="openedProject.store.id"
          :functionAst="currentMethodAst.value"
          :methodPointer="currentMethodPointer"
        />
      </template>
    </MarkdownEditor>
    <!-- Specifying `<ResultComponent ... centered /> does not work with React components
      `="true"` must be there-->
    <ResultComponent
      v-else
      status="info"
      :title="editorMarkdown.error.message('')"
      :centered="true"
    />
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
