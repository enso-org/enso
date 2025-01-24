<script setup lang="ts">
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import { transformPastedText } from '@/components/DocumentationEditor/textPaste'
import { ensoMarkdown } from '@/components/MarkdownEditor/markdown'
import VueHostRender, { VueHost } from '@/components/VueHostRender.vue'
import { useCodeMirror } from '@/util/codemirror'
import { highlightStyle } from '@/util/codemirror/highlight'
import { useLinkTitles } from '@/util/codemirror/links'
import { Vec2 } from '@/util/data/vec2'
import { EditorView } from '@codemirror/view'
import { minimalSetup } from 'codemirror'
import { computed, onMounted, ref, useCssModule, useTemplateRef, type ComponentInstance } from 'vue'
import * as Y from 'yjs'

const { content } = defineProps<{
  content: Y.Text | string
  toolbarContainer?: HTMLElement | undefined
}>()

const focused = ref(false)
const editing = computed(() => !readonly.value && focused.value)

const vueHost = new VueHost()
const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorRoot>>('editorRoot')
const { editorView, readonly, putTextAt } = useCodeMirror(editorRoot, {
  content: () => content,
  extensions: [
    minimalSetup,
    EditorView.lineWrapping,
    highlightStyle(useCssModule()),
    EditorView.clipboardInputFilter.of(transformPastedText),
    ensoMarkdown(),
  ],
  vueHost: () => vueHost,
})

useLinkTitles(editorView, { readonly })

onMounted(() => {
  // Enable rendering the line containing the current cursor in `editing` mode if focus enters the element *inside* the
  // scroll area--if we attached the handler to the editor root, clicking the scrollbar would cause editing mode to be
  // activated.
  editorView.dom
    .getElementsByClassName('cm-content')[0]!
    .addEventListener('focusin', () => (focused.value = true))
})

defineExpose({
  putText: (text: string) => {
    const range = editorView.state.selection.main
    putTextAt(text, range.from, range.to)
  },
  putTextAt,
  putTextAtCoords: (text: string, coords: Vec2) => {
    const pos = editorView.posAtCoords(coords, false)
    putTextAt(text, pos, pos)
  },
})
</script>

<template>
  <CodeMirrorRoot
    ref="editorRoot"
    v-bind="$attrs"
    :class="{ editing }"
    @focusout="focused = false"
  />
  <VueHostRender :host="vueHost" />
</template>

<style scoped>
:deep(.cm-content) {
  font-family: var(--font-sans);
}

:deep(.cm-editor) {
  opacity: 1;
  color: black;
  font-size: 12px;
}

:deep(img.uploading) {
  opacity: 0.5;
}
</style>

<!--suppress CssUnusedSymbol -->
<style module>
/* === Syntax styles === */

.heading1 {
  font-weight: 700;
  font-size: 20px;
  line-height: 1.75;
}

.heading2 {
  font-weight: 700;
  font-size: 16px;
  line-height: 1.75;
}

.heading3,
.heading4,
.heading5,
.heading6 {
  font-size: 14px;
  line-height: 2;
}

.processingInstruction {
  opacity: 20%;
}

.emphasis:not(.processingInstruction) {
  font-style: italic;
}

.strong:not(.processingInstruction) {
  font-weight: bold;
}

.strikethrough:not(.processingInstruction) {
  text-decoration: line-through;
}

.monospace {
  /*noinspection CssNoGenericFontName*/
  font-family: var(--font-mono);
}

.url {
  color: royalblue;
}

/* === View-mode === */

:global(.MarkdownEditor:not(.editing) .cm-line),
:global(.MarkdownEditor .cm-line:not(.cm-has-cursor)) {
  :global(.cm-image-markup) {
    display: none;
  }

  .processingInstruction {
    display: none;
  }

  .link:not(a *) {
    display: none;
  }

  a {
    cursor: pointer;
    color: blue;

    &:hover {
      text-decoration: underline;
    }
  }

  &:has(.list.processingInstruction) {
    display: list-item;
    list-style-type: disc;
    list-style-position: inside;
  }
}
</style>
