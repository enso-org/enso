<script setup lang="ts">
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import { linkifyUrls } from '@/components/PlainTextEditor/linkifyUrls'
import VueHostRender, { VueHostInstance } from '@/components/VueHostRender.vue'
import { useCodeMirror } from '@/util/codemirror'
import { useLinkTitles } from '@/util/codemirror/links'
import type { Extension } from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { useTemplateRef, type ComponentInstance } from 'vue'

const {
  extensions = [],
  readonly = false,
  contentTestId,
  editorReadyCallback = () => {},
} = defineProps<{
  extensions?: Extension | undefined
  readonly?: boolean | undefined
  contentTestId?: string | undefined
  editorReadyCallback: (view: EditorView) => void
}>()

const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorRoot>>('editorRoot')
const vueHost = new VueHostInstance()
const { editorView, contentElement } = useCodeMirror(editorRoot, {
  extensions: [linkifyUrls, EditorView.lineWrapping, extensions],
  vueHost: () => vueHost,
  contentTestId,
  lineMode: 'single',
})

useLinkTitles(editorView, { readonly })

editorReadyCallback(editorView)

defineExpose({
  contentElement,
})
</script>

<template>
  <CodeMirrorRoot ref="editorRoot" @keydown.enter.stop @keydown.up.stop @keydown.down.stop>
    <VueHostRender :host="vueHost" />
  </CodeMirrorRoot>
</template>

<style scoped>
:deep(a) {
  color: lightskyblue;
}
</style>
