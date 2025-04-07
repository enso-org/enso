<script setup lang="ts">
import CodeMirrorInlineRoot from '@/components/CodeMirrorInlineRoot.vue'
import { linkifyUrls } from '@/components/PlainTextEditor/linkifyUrls'
import VueHostRender, { VueHostInstance } from '@/components/VueHostRender.vue'
import { useCodeMirror } from '@/util/codemirror'
import { useLinkTitles } from '@/util/codemirror/links'
import { useTemplateRef, type ComponentInstance } from 'vue'
import * as Y from 'yjs'

const { content, contentTestId } = defineProps<{
  content: Y.Text | string
  contentTestId?: string | undefined
}>()

const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorInlineRoot>>('editorRoot')
const vueHost = new VueHostInstance()
const { editorView, readonly, contentElement } = useCodeMirror(editorRoot, {
  content: () => content,
  extensions: [linkifyUrls],
  vueHost: () => vueHost,
  contentTestId,
  singleLine: true,
})

useLinkTitles(editorView, { readonly })

defineExpose({
  contentElement,
})
</script>

<template>
  <CodeMirrorInlineRoot ref="editorRoot" @keydown.enter.stop>
    <VueHostRender :host="vueHost" />
  </CodeMirrorInlineRoot>
</template>

<style scoped>
:deep(a) {
  color: lightskyblue;
}
</style>
