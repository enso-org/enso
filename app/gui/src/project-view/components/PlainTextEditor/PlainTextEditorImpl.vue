<script setup lang="ts">
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import { linkifyUrls } from '@/components/PlainTextEditor/linkifyUrls'
import VueHostRender, { VueHost } from '@/components/VueHostRender.vue'
import { useCodeMirror } from '@/util/codemirror'
import { useLinkTitles } from '@/util/codemirror/links'
import { useTemplateRef, type ComponentInstance } from 'vue'
import * as Y from 'yjs'

const { content } = defineProps<{ content: Y.Text | string }>()

const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorRoot>>('editorRoot')
const vueHost = new VueHost()
const { editorView, readonly, contentElement } = useCodeMirror(editorRoot, {
  content: () => content,
  extensions: [linkifyUrls],
  vueHost: () => vueHost,
})

useLinkTitles(editorView, { readonly })

defineExpose({
  contentElement,
})
</script>

<template>
  <CodeMirrorRoot ref="editorRoot" v-bind="$attrs" />
  <VueHostRender :host="vueHost" />
</template>

<style scoped>
:deep(a) {
  color: lightskyblue;
}
</style>
