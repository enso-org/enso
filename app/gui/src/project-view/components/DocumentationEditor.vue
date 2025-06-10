<script setup lang="ts">
import { injectCurrentProject } from '$/components/WithCurrentProject.vue'
import { useBackends } from '$/providers/backends'
import { useRightPanelData } from '$/providers/rightPanel'
import { documentationEditorBindings } from '@/bindings'
import { resolveDocImageUrl, useDocumentationImages } from '@/components/DocumentationEditor/images'
import { transformPastedText } from '@/components/DocumentationEditor/textPaste'
import FullscreenButton from '@/components/FullscreenButton.vue'
import FunctionSignatureEditor from '@/components/FunctionSignatureEditor.vue'
import MarkdownEditor from '@/components/MarkdownEditor.vue'
import { htmlToMarkdown } from '@/components/MarkdownEditor/htmlToMarkdown'
import SvgButton from '@/components/SvgButton.vue'
import { useProjectFiles } from '@/stores/projectFiles'
import { MutableFunctionDef, parseModule } from '@/util/ast/abstract'
import { Err, mapOk, Ok, unwrapOr } from '@/util/data/result'
import { methodPointerEquals } from '@/util/methodPointer'
import { ResultComponent } from '@/util/react'
import { useQuery } from '@tanstack/vue-query'
import { ComponentInstance, computed, effectScope, ref, watch } from 'vue'
import { prerenderMarkdown } from 'ydoc-shared/ast/documentation'

const markdownEditor = ref<ComponentInstance<typeof MarkdownEditor>>()

const rightPanel = useRightPanelData()
const openedProject = injectCurrentProject().ref
const projectId = computed(() => rightPanel.focusedProject)
const { backendForType } = useBackends()
const backendForAsset = computed(() => {
  if (rightPanel.context?.category == null) return null
  return backendForType(rightPanel.context.category.backend)
})

const fileContentsFromCloud = useQuery({
  queryKey: computed(
    () =>
      [
        backendForAsset.value?.type,
        {
          method: 'getFileContent',
          projectId: projectId.value,
        },
      ] as const,
  ),
  enabled: computed(
    () => openedProject.value == null && backendForAsset.value != null && projectId.value != null,
  ),
  queryFn: ({ queryKey }) => {
    const [, { projectId }] = queryKey
    return projectId && backendForAsset.value?.getFileContent(projectId)
  },
})

const currentMethodAst = computed(() => {
  if (openedProject.value) {
    return mapOk(openedProject.value.graph.currentMethod.ast, (ast) => ({ ast, readOnly: false }))
  } else if (fileContentsFromCloud.data != null) {
    if (fileContentsFromCloud.error.value) return Err(fileContentsFromCloud.error.value)
    if (fileContentsFromCloud.isLoading.value) return Err('Loading documentation...')
    const code = fileContentsFromCloud.data.value
    if (code) {
      const module = parseModule(code)
      for (const statement of module.statements()) {
        if (statement instanceof MutableFunctionDef && statement.name.code() === 'main') {
          return Ok({ ast: statement, readOnly: true })
        }
      }
    }
  }
  return Err('No documentation available')
})

const markdownDocs = computed(() => {
  if (!currentMethodAst.value.ok) return currentMethodAst.value
  const docs = currentMethodAst.value.value.ast.mutableDocumentationMarkdown()
  if (currentMethodAst.value.value.readOnly) {
    return Ok(docs.toJSON())
  } else {
    return Ok(docs)
  }
})

const isEditable = computed(
  () => currentMethodAst.value.ok && !currentMethodAst.value.value.readOnly,
)

const docImagesHandlers = ref<ReturnType<typeof useDocumentationImages>>()

watch(
  openedProject,
  (openedProject, _, onCleanup) => {
    const scope = effectScope()
    scope.run(() => {
      if (openedProject != null) {
        const { store, graph } = openedProject
        docImagesHandlers.value = useDocumentationImages(
          () => (markdownEditor.value?.loaded ? markdownEditor.value : undefined),
          computed(() => graph.modulePath),
          useProjectFiles(store),
        )
      } else {
        docImagesHandlers.value = {
          transformImageUrl: (path: string) => {
            if (backendForAsset.value == null) return Promise.resolve(Err('No backend available'))
            if (projectId.value == null) return Promise.resolve(Err('No project selected'))
            // In Enso Documentation, the relative paths are from module's directory
            // Here we always display docs from `src/Main.enso` module
            const resolvedUrl = resolveDocImageUrl(['src'], path)
            if (!resolvedUrl.ok) return Promise.resolve(resolvedUrl)
            if (resolvedUrl.value.type === 'url') {
              return Promise.resolve(Ok({ url: resolvedUrl.value.url.toString() }))
            } else {
              return backendForAsset.value
                .resolveProjectAssetPath(projectId.value, resolvedUrl.value.path)
                .then(
                  (url) => Ok({ url }),
                  (error) => {
                    console.error(error)
                    return Err(error)
                  },
                )
            }
          },
          tryUploadImageFile: async () => {},
          tryUploadDroppedImage: async () => {},
          tryUploadPastedImage: () => {
            return false
          },
        }
      }
    })
    onCleanup(() => scope.stop())
  },
  { immediate: true },
)

function handlePaste(raw: boolean) {
  window.navigator.clipboard.read().then(async (items) => {
    if (!markdownEditor.value) return
    for (const item of items) {
      if (docImagesHandlers.value?.tryUploadPastedImage(item)) continue
      const htmlType = item.types.find((type) => type === 'text/html')
      if (htmlType) {
        const blob = await item.getType(htmlType)
        const html = await blob.text()
        const markdown = prerenderMarkdown(await htmlToMarkdown(html))
        markdownEditor.value.putText(markdown)
        continue
      }
      const textType = item.types.find((type) => type === 'text/plain')
      if (textType) {
        const blob = await item.getType(textType)
        const rawText = await blob.text()
        markdownEditor.value.putText(raw ? rawText : transformPastedText(rawText))
      }
    }
  })
}

const handler = documentationEditorBindings.handler({
  'documentationEditor.paste': () => handlePaste(false),
  'documentationEditor.pasteRaw': () => handlePaste(true),
})

const currentMethodPointer = computed(
  () => openedProject.value && unwrapOr(openedProject.value.graph.currentMethod.pointer, undefined),
)
const displaySignatureEditor = computed(
  () =>
    currentMethodPointer.value &&
    openedProject.value?.store.entryPoint &&
    !methodPointerEquals(currentMethodPointer.value, openedProject.value.store.entryPoint),
)
</script>

<template>
  <div
    class="DocumentationEditor"
    @keydown="handler"
    @dragover.prevent
    @drop.prevent="docImagesHandlers?.tryUploadDroppedImage($event)"
  >
    <MarkdownEditor
      v-if="markdownDocs.ok"
      ref="markdownEditor"
      :content="markdownDocs.value"
      :transformImageUrl="docImagesHandlers?.transformImageUrl"
      contentTestId="documentation-editor-content"
    >
      <template #toolbarLeft>
        <FullscreenButton v-model="rightPanel.fullscreen" />
      </template>
      <template #toolbarRight>
        <SvgButton
          v-if="isEditable"
          name="image"
          title="Insert image"
          @activate="docImagesHandlers?.tryUploadImageFile()"
        />
      </template>
      <template #belowToolbar>
        <FunctionSignatureEditor
          v-if="displaySignatureEditor && currentMethodAst.ok && openedProject"
          :projectId="openedProject.store.id"
          :functionAst="currentMethodAst.value.ast"
          :methodPointer="currentMethodPointer"
          :markdownDocs="markdownDocs.value"
        />
      </template>
    </MarkdownEditor>
    <!-- Specifying `<ResultComponent ... centered /> does not work with React components
      `="true"` must be there-->
    <ResultComponent
      v-else
      status="info"
      :title="markdownDocs.error.message('')"
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

.FullscreenButton {
  margin-left: 4px;
}
</style>
