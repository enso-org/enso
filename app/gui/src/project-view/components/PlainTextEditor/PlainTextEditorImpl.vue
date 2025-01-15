<script setup lang="ts">
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import { linkifyUrls } from '@/components/PlainTextEditor/linkifyUrls'
import VueComponentHost from '@/components/VueComponentHost.vue'
import { useCodeMirror } from '@/util/codemirror'
import { useLinkTitles } from '@/util/codemirror/links'
import { useTemplateRef, type ComponentInstance } from 'vue'
import * as Y from 'yjs'

const { content } = defineProps<{ content: Y.Text | string }>()

const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorRoot>>('editorRoot')
const vueHost = useTemplateRef<ComponentInstance<typeof VueComponentHost>>('vueHost')
const { editorView, readonly, contentElement } = useCodeMirror(editorRoot, {
  content: () => content,
  extensions: [linkifyUrls],
  vueHost: () => vueHost.value || undefined,
})

useLinkTitles(editorView, { readonly })

defineExpose({
  contentElement,
})
</script>

<template>
  <CodeMirrorRoot ref="editorRoot" v-bind="$attrs" />
  <VueComponentHost ref="vueHost" />
</template>

<style scoped>
:deep(a) {
  color: lightskyblue;
}
</style>
