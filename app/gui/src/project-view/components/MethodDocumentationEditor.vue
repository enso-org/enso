<script setup lang="ts">
import { ProjectId } from '#/services/Backend'
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import FunctionSignatureEditor from '@/components/FunctionSignatureEditor.vue'
import MarkdownEditor from '@/components/MarkdownEditor.vue'
import { provideDocumentationImages } from '@/components/MarkdownEditor/imageFiles'
import { Ast } from '@/util/ast'
import { useYTextSync } from '@/util/codemirror'
import { MethodPointer, methodPointerEquals } from '@/util/methodPointer'
import { EditorView } from '@codemirror/view'
import { computed } from 'vue'

const props = defineProps<{
  projectId: ProjectId
  method: Ast.FunctionDef
  pointer: MethodPointer | undefined
}>()

const openedProject = useCurrentProject().ref

// const currentMethodPointer = computed(
//   () => openedProject.value && unwrapOr(openedProject.value.graph.currentMethod.pointer, undefined),
// )
const displaySignatureEditor = computed(
  () =>
    props.pointer &&
    openedProject.value?.store.entryPoint &&
    !methodPointerEquals(props.pointer, openedProject.value.store.entryPoint),
)

const editorMarkdown = computed(() => props.method.mutableDocumentationMarkdown())
//   if (currentMethodAst.value != null) {
//     return mapOk(currentMethodAst.value, (ast) => ast.mutableDocumentationMarkdown())
//   } else {
//     return Err('No documentation available')
//   }
// })

const syncExt = (view: EditorView) => {
  const { syncExt, connectSync } = useYTextSync(editorMarkdown)
  connectSync(view)
  return syncExt
}

provideDocumentationImages({
  openedProject,
  backend: null,
  projectId: props.projectId,
})
</script>

<template>
  <!-- <div class="DocumentationEditor"> -->
  <MarkdownEditor contentTestId="documentation-editor-content" :extensions="syncExt">
    <template #belowToolbar>
      <FunctionSignatureEditor
        v-if="displaySignatureEditor"
        :projectId="projectId"
        :functionAst="method"
        :methodPointer="pointer"
      />
    </template>
  </MarkdownEditor>
  <!-- </div> -->
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
