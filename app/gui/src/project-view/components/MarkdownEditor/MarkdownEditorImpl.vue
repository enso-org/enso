<script setup lang="ts">
import ActionButton from '@/components/ActionButton.vue'
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import { transformPastedText } from '@/components/DocumentationEditor/textPaste'
import BlockTypeDropdown from '@/components/MarkdownEditor/BlockTypeDropdown.vue'
import { ensoMarkdown, useMarkdownFormatting } from '@/components/MarkdownEditor/codemirror'
import { type BlockType } from '@/components/MarkdownEditor/codemirror/formatting'
import { useFormatActions } from '@/components/MarkdownEditor/formatActions'
import VueHostRender, { VueHostInstance } from '@/components/VueHostRender.vue'
import { useCodeMirror } from '@/util/codemirror'
import { highlightStyle } from '@/util/codemirror/highlight'
import { useLinkTitles } from '@/util/codemirror/links'
import { Vec2 } from '@/util/data/vec2'
import { defaultHighlightStyle, syntaxHighlighting } from '@codemirror/language'
import { drawSelection, EditorView } from '@codemirror/view'
import { type ComponentInstance, computed, ref, useCssModule, useTemplateRef } from 'vue'
import * as Y from 'yjs'

const { content, toolbar, contentTestId } = defineProps<{
  content: Y.Text | string
  toolbar: boolean
  contentTestId?: string | undefined
}>()
defineOptions({
  inheritAttrs: false,
})

function useEditorFocus(view: EditorView) {
  const focused = ref(false)
  const focusHandlers = {
    focusin: (event: FocusEvent) => {
      // Enable rendering the line containing the current cursor in `editing` mode if focus enters
      // the element *inside* the scroll area--if we handled the event for the editor root, clicking
      // the scrollbar would cause editing mode to be activated.
      if (event.target instanceof Node && view.contentDOM.contains(event.target))
        focused.value = true
    },
    focusout: () => {
      // If the focus leaves the whole editor, we exit editing mode. Note the asymmetry with
      // `onFocusIn`: This way, clicking the scrollbar doesn't change edit mode.
      focused.value = false
    },
  }
  return { focused, focusHandlers }
}

const vueHost = new VueHostInstance()
const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorRoot>>('editorRoot')
const { editorView, readonly, putTextAt, setExtraExtensions } = useCodeMirror(editorRoot, {
  content: () => content,
  extensions: [
    drawSelection(),
    syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
    EditorView.lineWrapping,
    highlightStyle(useCssModule()),
    EditorView.clipboardInputFilter.of(transformPastedText),
    ensoMarkdown(),
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
})
setExtraExtensions([formatBindings])

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
  <div class="MarkdownEditorRoot">
    <div v-if="toolbar" class="toolbar" @pointerdown.prevent>
      <slot name="toolbarLeft" />
      <template v-if="!readonly">
        <BlockTypeDropdown
          :modelValue="formatting.blockType.value ?? 'Unknown'"
          @update:modelValue="formatting.blockType.set($event as BlockType)"
        />
        <ActionButton action="documentationEditor.italic" />
        <ActionButton action="documentationEditor.bold" />
        <ActionButton action="documentationEditor.link" />
        <ActionButton action="documentationEditor.code" />
      </template>
      <slot name="toolbarRight" />
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

  /*noinspection CssUnusedSymbol*/
  & :deep(img.uploading) {
    opacity: 0.5;
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
