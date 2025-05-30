<script setup lang="ts">
import { documentationEditorBindings } from '@/bindings'
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import { transformPastedText } from '@/components/DocumentationEditor/textPaste'
import BlockTypeDropdown from '@/components/MarkdownEditor/BlockTypeDropdown.vue'
import { ensoMarkdown, useMarkdownFormatting } from '@/components/MarkdownEditor/codemirror'
import { type BlockType } from '@/components/MarkdownEditor/codemirror/formatting'
import SvgButton from '@/components/SvgButton.vue'
import ToggleIcon from '@/components/ToggleIcon.vue'
import VueHostRender, { VueHostInstance } from '@/components/VueHostRender.vue'
import { useCodeMirror } from '@/util/codemirror'
import { highlightStyle } from '@/util/codemirror/highlight'
import { useLinkTitles } from '@/util/codemirror/links'
import { Vec2 } from '@/util/data/vec2'
import { useToast } from '@/util/toast'
import { defaultHighlightStyle, syntaxHighlighting } from '@codemirror/language'
import { drawSelection, EditorView } from '@codemirror/view'
import { computed, onMounted, ref, useCssModule, useTemplateRef, type ComponentInstance } from 'vue'
import * as Y from 'yjs'

const { content, toolbar, contentTestId } = defineProps<{
  content: Y.Text | string
  toolbar: boolean
  contentTestId?: string | undefined
}>()
defineOptions({
  inheritAttrs: false,
})

const toastError = useToast.error()

const focused = ref(false)
const editing = computed(() => !readonly.value && focused.value)

const vueHost = new VueHostInstance()
const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorRoot>>('editorRoot')
const { editorView, readonly, putTextAt } = useCodeMirror(editorRoot, {
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
const { italic, bold, insertLink, blockType, insertCodeBlock } = useMarkdownFormatting(editorView)

useLinkTitles(editorView, { readonly })

onMounted(() => {
  // Enable rendering the line containing the current cursor in `editing` mode if focus enters the element *inside* the
  // scroll area--if we attached the handler to the editor root, clicking the scrollbar would cause editing mode to be
  // activated.
  editorView.dom
    .getElementsByClassName('cm-content')[0]!
    .addEventListener('focusin', () => (focused.value = true))
})

function reportUnformattable() {
  toastError.show('The selected text cannot be formated')
}

function toggleFormat({
  set,
  value,
}: {
  set: ((value: boolean) => void) | undefined
  value: boolean
}) {
  if (!set) {
    reportUnformattable()
    return
  }
  set(!value)
}

function doFormat(action: (() => void) | undefined) {
  if (!action) {
    reportUnformattable()
    return
  }
  action()
}

function binding(binding: keyof typeof documentationEditorBindings.bindings): string {
  return documentationEditorBindings.bindings[binding].humanReadable
}

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
  bold: () => toggleFormat(bold),
  italic: () => toggleFormat(italic),
  header1: () => blockType.set('ATXHeading1'),
  header2: () => blockType.set('ATXHeading2'),
  header3: () => blockType.set('ATXHeading3'),
  paragraph: () => blockType.set('Paragraph'),
  link: () => doFormat(insertLink.value),
})
</script>

<template>
  <div class="MarkdownEditorRoot">
    <div v-if="toolbar" class="toolbar" @pointerdown.prevent>
      <slot name="toolbarLeft" />
      <template v-if="!readonly">
        <BlockTypeDropdown
          :modelValue="blockType.value ?? 'Unknown'"
          @update:modelValue="blockType.set($event as BlockType)"
        />
        <ToggleIcon
          icon="italic"
          :disabled="!editing || !italic.set"
          :modelValue="italic.value"
          :title="`Italic (${binding('italic')})`"
          @update:modelValue="italic.set!"
        />
        <ToggleIcon
          icon="bold"
          :disabled="!editing || !bold.set"
          :modelValue="bold.value"
          :title="`Bold (${binding('bold')})`"
          @update:modelValue="bold.set!"
        />
        <SvgButton
          name="connector_add"
          :disabled="insertLink == null"
          :title="`Insert link (${binding('link')})`"
          @activate="insertLink?.()"
        />
        <SvgButton
          name="code"
          :disabled="insertCodeBlock == null"
          title="Insert code block"
          @activate="insertCodeBlock?.()"
        />
      </template>
      <slot name="toolbarRight" />
    </div>
    <slot name="belowToolbar" />
    <CodeMirrorRoot
      ref="editorRoot"
      v-bind="$attrs"
      :class="{ editing }"
      @focusout="focused = false"
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
