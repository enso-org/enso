<script setup lang="ts">
import { Result as ResultReact } from '#/components/Result'
import { ProjectId } from '#/services/Backend'
import { injectCurrentProject } from '$/components/WithCurrentProject.vue'
import { injectBackends } from '$/providers/backends'
import { injectConainerData } from '$/providers/container'
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
import { Err, mapOk, Ok, Result, unwrapOr } from '@/util/data/result'
import { methodPointerEquals } from '@/util/methodPointer'
import { useQuery } from '@tanstack/vue-query'
import { applyPureReactInVue } from 'veaury'
import { ComponentInstance, computed, effectScope, ref, watch } from 'vue'
import { prerenderMarkdown } from 'ydoc-shared/ast/documentation'

const Result = applyPureReactInVue(ResultReact)
const markdownEditor = ref<ComponentInstance<typeof MarkdownEditor>>()

const { rightPanel } = injectConainerData()
const { id: openedProjectId, store: projectStore, graph } = injectCurrentProject()
const projectId = computed(() => rightPanel.focusedProject)
const { backendForType } = injectBackends()
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
          projectId: projectId.value as ProjectId,
        },
      ] as const,
  ),
  enabled: computed(
    () => graph.value == null && backendForAsset.value != null && projectId.value != null,
  ),
  queryFn: ({ queryKey }) => {
    const [, { projectId }] = queryKey
    return projectId && backendForAsset.value?.getFileContent(projectId)
  },
})

const currentMethodAst = computed(() => {
  if (graph.value) {
    return mapOk(graph.value.currentMethod.ast, (ast) => ({ ast, readOnly: false }))
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

const docImagesHandlers = ref<ReturnType<typeof useDocumentationImages>>()

watch(
  [projectStore, graph],
  ([projectStore, graph], _, onCleanup) => {
    const scope = effectScope()
    scope.run(() => {
      if (projectStore != null && graph != null) {
        docImagesHandlers.value = useDocumentationImages(
          () => (markdownEditor.value?.loaded ? markdownEditor.value : undefined),
          computed(() => graph.modulePath),
          useProjectFiles(projectStore),
        )
      } else {
        docImagesHandlers.value = {
          transformImageUrl: (path: string) => {
            // In Enso Documentation, the relative paths are from module's directory
            // Here we always display docs from `src/Main.enso` module
            if (backendForAsset.value == null) return Promise.resolve(Err('No backend available'))
            if (projectId.value == null) return Promise.resolve(Err('No project selected'))
            const resolvedUrl = resolveDocImageUrl(['src'], path)
            if (!resolvedUrl.ok) return Promise.resolve(resolvedUrl)
            if (resolvedUrl.value.type === 'url') {
              return Promise.resolve(Ok({ url: resolvedUrl.value.url.toString() }))
            } else {
              return backendForAsset.value
                .resolveProjectAssetPath(projectId.value as ProjectId, resolvedUrl.value.path)
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
  paste: () => handlePaste(false),
  pasteRaw: () => handlePaste(true),
  bold: () => markdownEditor.value?.bold(),
  italic: () => markdownEditor.value?.italic(),
  header1: () => markdownEditor.value?.header1(),
  header2: () => markdownEditor.value?.header2(),
  header3: () => markdownEditor.value?.header3(),
  paragraph: () => markdownEditor.value?.paragraph(),
  link: () => markdownEditor.value?.link(),
})

const currentMethodPointer = computed(
  () => graph.value && unwrapOr(graph.value.currentMethod.pointer, undefined),
)
const displaySignatureEditor = computed(
  () =>
    currentMethodPointer.value &&
    projectStore.value?.entryPoint &&
    !methodPointerEquals(currentMethodPointer.value, projectStore.value.entryPoint),
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
          name="image"
          title="Insert image"
          @activate="docImagesHandlers?.tryUploadImageFile()"
        />
      </template>
      <template #belowToolbar>
        <FunctionSignatureEditor
          v-if="displaySignatureEditor && currentMethodAst.ok && openedProjectId"
          :projectId="openedProjectId"
          :functionAst="currentMethodAst.value.ast"
          :methodPointer="currentMethodPointer"
          :markdownDocs="markdownDocs.value"
        />
      </template>
    </MarkdownEditor>
    <Result v-else status="info" :title="markdownDocs.error.message('')" centered />
  </div>
</template>

<style scoped>
.DocumentationEditor {
  display: flex;
  flex-direction: column;
  background-color: #fff;
  height: 100%;
  width: 100%;
  padding-left: 16px;
  padding-right: 2px;
}
</style>
