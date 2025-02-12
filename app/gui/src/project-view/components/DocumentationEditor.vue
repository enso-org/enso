<script setup lang="ts">
import { documentationEditorBindings } from '@/bindings'
import { useDocumentationImages } from '@/components/DocumentationEditor/images'
import { transformPastedText } from '@/components/DocumentationEditor/textPaste'
import FullscreenButton from '@/components/FullscreenButton.vue'
import MarkdownEditor from '@/components/MarkdownEditor.vue'
import { htmlToMarkdown } from '@/components/MarkdownEditor/htmlToMarkdown'
import SvgButton from '@/components/SvgButton.vue'
import WithFullscreenMode from '@/components/WithFullscreenMode.vue'
import { useGraphStore } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import { useProjectFiles } from '@/stores/projectFiles'
import { ComponentInstance, ref, toRef, watch } from 'vue'
import { normalizeMarkdown } from 'ydoc-shared/ast/documentation'
import * as Y from 'yjs'

const { yText } = defineProps<{
  yText: Y.Text
}>()
const emit = defineEmits<{
  'update:fullscreen': [boolean]
}>()

const markdownEditor = ref<ComponentInstance<typeof MarkdownEditor>>()

const graphStore = useGraphStore()
const projectStore = useProjectStore()
const { transformImageUrl, tryUploadPastedImage, tryUploadDroppedImage, tryUploadImageFile } =
  useDocumentationImages(
    () => (markdownEditor.value?.loaded ? markdownEditor.value : undefined),
    toRef(graphStore, 'modulePath'),
    useProjectFiles(projectStore),
  )

const fullscreen = ref(false)
const fullscreenAnimating = ref(false)

watch(
  () => fullscreen.value || fullscreenAnimating.value,
  (fullscreenOrAnimating) => emit('update:fullscreen', fullscreenOrAnimating),
)

function handlePaste(raw: boolean) {
  window.navigator.clipboard.read().then(async (items) => {
    if (!markdownEditor.value) return
    for (const item of items) {
      if (tryUploadPastedImage(item)) continue
      const htmlType = item.types.find((type) => type === 'text/html')
      if (htmlType) {
        const blob = await item.getType(htmlType)
        const html = await blob.text()
        const markdown = normalizeMarkdown(await htmlToMarkdown(html))
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
})
</script>

<template>
  <WithFullscreenMode :fullscreen="fullscreen" @update:animating="fullscreenAnimating = $event">
    <div
      class="DocumentationEditor"
      @keydown="handler"
      @dragover.prevent
      @drop.prevent="tryUploadDroppedImage($event)"
    >
      <MarkdownEditor ref="markdownEditor" :content="yText" :transformImageUrl="transformImageUrl">
        <template #toolbarLeft>
          <FullscreenButton v-model="fullscreen" />
        </template>
        <template #toolbarRight>
          <SvgButton name="image" title="Insert image" @click.stop="tryUploadImageFile()" />
        </template>
        <template #belowToolbar>
          <slot name="belowToolbar" />
        </template>
      </MarkdownEditor>
    </div>
  </WithFullscreenMode>
</template>

<style scoped>
.DocumentationEditor {
  display: flex;
  flex-direction: column;
  background-color: #fff;
  height: 100%;
  width: 100%;
}
</style>
