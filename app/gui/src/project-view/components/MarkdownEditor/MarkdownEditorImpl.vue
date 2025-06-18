<script setup lang="ts">
import ActionButton from '@/components/ActionButton.vue'
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import BlockTypeDropdown from '@/components/MarkdownEditor/BlockTypeDropdown.vue'
import { ensoMarkdown, useMarkdownFormatting } from '@/components/MarkdownEditor/codemirror'
import type { BlockType } from '@/components/MarkdownEditor/codemirror/formatting'
import { useFormatActions } from '@/components/MarkdownEditor/formatActions'
import { useDocumentationImages } from '@/components/MarkdownEditor/imageFiles'
import VueHostRender, { VueHostInstance } from '@/components/VueHostRender.vue'
import { useCodeMirror, useEditorFocus } from '@/util/codemirror'
import { highlightStyle } from '@/util/codemirror/highlight'
import { useLinkTitles } from '@/util/codemirror/links'
import { defaultHighlightStyle, syntaxHighlighting } from '@codemirror/language'
import { drawSelection, EditorView } from '@codemirror/view'
import { type ComponentInstance, computed, useCssModule, useTemplateRef } from 'vue'
import * as Y from 'yjs'

const { content, toolbar, contentTestId } = defineProps<{
  content: Y.Text | string
  toolbar: boolean
  contentTestId?: string | undefined
}>()
defineOptions({
  inheritAttrs: false,
})

const images = useDocumentationImages(true)

const vueHost = new VueHostInstance()
const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorRoot>>('editorRoot')
const { editorView, readonly, setExtraExtensions } = useCodeMirror(editorRoot, {
  content: () => content,
  extensions: [
    drawSelection(),
    syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
    EditorView.lineWrapping,
    highlightStyle(useCssModule()),
    ensoMarkdown({
      tryUploadPastedImage: () =>
        images?.value &&
        ((item: ClipboardItem) => images.value.tryUploadPastedImage(editorView, item)),
    }),
  ],
  vueHost: () => vueHost,
  lineMode: 'multi',
  contentTestId,
})

useLinkTitles(editorView, { readonly })

const { focused, focusHandlers } = useEditorFocus(editorView)
const editing = computed(() => !readonly.value && focused.value)

const formatting = useMarkdownFormatting(editorView)
const { formatBindings } = useFormatActions({
  formatting,
  editing,
  uploadImage: () => images?.value && (() => images.value.tryUploadImageFile(editorView)),
})
setExtraExtensions([formatBindings])
</script>

<template>
  <div
    class="MarkdownEditorRoot"
    @dragover.prevent
    @drop.prevent="images?.tryUploadDroppedImage?.(editorView, $event)"
  >
    <div v-if="toolbar" class="toolbar" @pointerdown.prevent>
      <ActionButton action="panel.fullscreen" />
      <template v-if="!readonly">
        <BlockTypeDropdown
          :modelValue="formatting.blockType.value ?? 'Unknown'"
          @update:modelValue="formatting.blockType.set($event as BlockType)"
        />
        <ActionButton action="documentationEditor.italic" />
        <ActionButton action="documentationEditor.bold" />
        <ActionButton action="documentationEditor.link" />
        <ActionButton action="documentationEditor.code" />
        <ActionButton action="documentationEditor.image" />
      </template>
    </div>
    <slot name="belowToolbar" />
    <CodeMirrorRoot
      ref="editorRoot"
      v-bind="$attrs"
      :class="{ editing }"
      v-on="focusHandlers"
      @keydown.enter.stop
    >
      <VueHostRender :host="vueHost" />
    </CodeMirrorRoot>
  </div>
</template>

<style scoped>
.MarkdownEditorRoot {
  display: flex;
  flex-direction: column;
  height: 100%;
  width: 100%;
  gap: 8px;
}

.toolbar {
  height: 26px;
  flex-shrink: 0;
  display: flex;
  align-items: center;
  flex-direction: row;
  gap: 8px;
}

/*noinspection CssUnusedSymbol*/
.CodeMirrorRoot {
  /*noinspection CssUnusedSymbol*/
  & :deep(.cm-content) {
    /*noinspection CssUnresolvedCustomProperty,CssNoGenericFontName*/
    font-family: var(--font-sans);
  }

  /*noinspection CssUnusedSymbol*/
  & :deep(.cm-line) {
    padding-left: 0;
    padding-right: 0;
  }

  /*noinspection CssUnusedSymbol*/
  & :deep(.cm-editor) {
    flex-grow: 1;

    opacity: 1;
    color: black;
    font-size: 12px;
  }
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

:global(.CodeMirrorRoot:not(.editing) .cm-line),
:global(.CodeMirrorRoot .cm-line:not(.cm-has-cursor)) {
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

  .list:not(*) {
    /* Hide indentation spaces */
    display: none;
  }

  :global(.cm-BulletList-item),
  :global(.cm-OrderedList-item) {
    display: list-item;
  }

  :global(.cm-BulletList-item) {
    list-style-type: disc;
    &:global(.cm-BulletList-item-odd) {
      list-style-type: circle;
    }
    list-style-position: outside;
    text-indent: -0.3em;
    /*noinspection CssUnresolvedCustomProperty*/
    margin-left: calc(var(--cm-list-depth) * 0.57em + 1em);
  }

  :global(.cm-OrderedList-item) {
    list-style-type: decimal;
    list-style-position: inside;
    /*noinspection CssUnresolvedCustomProperty*/
    margin-left: calc(var(--cm-list-depth) * 0.85em);
  }
}
</style>
