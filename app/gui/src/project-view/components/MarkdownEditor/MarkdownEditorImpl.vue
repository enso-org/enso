<script setup lang="ts">
import ActionButton from '@/components/ActionButton.vue'
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import { useBlockTypeDropdown } from '@/components/MarkdownEditor/blockTypeDropdown'
import { ensoMarkdown, useMarkdownFormatting } from '@/components/MarkdownEditor/codemirror'
import type { BlockType } from '@/components/MarkdownEditor/codemirror/formatting'
import {
  insertPlaceholder,
  replaceablePlaceholders,
  replacePlaceholder,
} from '@/components/MarkdownEditor/codemirror/placeholder'
import { useFormatActions } from '@/components/MarkdownEditor/formatActions'
import SelectionDropdown from '@/components/SelectionDropdown.vue'
import VueHostRender, { VueHostInstance } from '@/components/VueHostRender.vue'
import { type StartedUpload, useAsyncResources } from '@/providers/asyncResources'
import { useCurrentProjectResourceContext } from '@/providers/asyncResources/context'
import { type AnyUploadSource, selectResourceFiles } from '@/providers/asyncResources/upload'
import { useCodeMirror, useEditorFocus } from '@/util/codemirror'
import { highlightStyle } from '@/util/codemirror/highlight'
import { useLinkTitles } from '@/util/codemirror/links'
import { Vec2 } from '@/util/data/vec2'
import { useToast } from '@/util/toast'
import { defaultHighlightStyle, syntaxHighlighting } from '@codemirror/language'
import type { Extension } from '@codemirror/state'
import { drawSelection, EditorView } from '@codemirror/view'
import { type ComponentInstance, computed, useCssModule, useTemplateRef, watch } from 'vue'

const {
  toolbar = true,
  readonly = false,
  extensions = [],
  contentTestId,
  scrollerTestId,
  editorReadyCallback = () => {},
} = defineProps<{
  toolbar?: boolean | undefined
  readonly?: boolean | undefined
  /**
   * Additional extensions. This prop is read only during setup, and extensions are not refreshed
   * afterwards!
   */
  extensions?: Extension | undefined
  contentTestId?: string | undefined
  scrollerTestId?: string | undefined
  /**
   * A callback called when CodeMirror is set up, passing {@link EditorView}. It is called in this
   * component's setup, allowing creating watches bound to the editor view (that's why its not
   * defined as signal)
   */
  editorReadyCallback?: ((view: EditorView) => void) | undefined
}>()
defineOptions({ inheritAttrs: false })

const resourceContext = useCurrentProjectResourceContext()
const res = useAsyncResources(true)

async function selectAndUpload() {
  const files = await selectResourceFiles()
  if (files.ok) handleUpload(files.value)
}

const uploadErrorToast = useToast.error()

function handleUpload(source: AnyUploadSource): boolean {
  if (!res) return false
  const uploads = res.uploadResources(source, resourceContext)
  if (uploads.length == 0) return false

  const coords = source instanceof DragEvent ? new Vec2(source.clientX, source.clientY) : undefined
  insertStartedUploads(uploads, coords)
  return true
}

async function insertStartedUploads(uploads: Promise<StartedUpload>[], coords: Vec2 | undefined) {
  const selection = editorView.state.selection.main
  let from = coords ? editorView.posAtCoords(coords, false) : selection.from
  let to = coords ? from : selection.to

  for (const upload of uploads) {
    const placeholderText = `\n![]()\n`
    const placeholder = insertPlaceholder(editorView, from, to, placeholderText)
    // Set next placeholder insert position right after this one.
    from = to = from + placeholderText.length

    upload.then((result) => {
      // Once the upload metadata is known, fill in the placeholder.
      if (result.ok) {
        const { filename, resourceUrl, complete } = result.value
        const safeAltText = filename.replace(/\.([^.]+)$/, '').replace(/[[\]]/g, '_')

        const uploadText = `\n![${safeAltText}](${resourceUrl}?uploading)\n`
        const finalText = `\n![${safeAltText}](${resourceUrl})\n`

        replacePlaceholder(editorView, placeholder, uploadText, false)
        complete.then(() => replacePlaceholder(editorView, placeholder, finalText))
      } else {
        replacePlaceholder(editorView, placeholder, '')
        uploadErrorToast.reportError(result.error)
      }
    })
  }
}

const vueHost = new VueHostInstance()
const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorRoot>>('editorRoot')
const { editorView, setExtraExtensions } = useCodeMirror(editorRoot, {
  extensions: [
    drawSelection(),
    syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
    EditorView.lineWrapping,
    highlightStyle(useCssModule()),
    ensoMarkdown({ customClipboardAction: handleUpload, customDropAction: handleUpload }),
    replaceablePlaceholders,
    extensions,
  ],
  readonly: () => readonly,
  vueHost: () => vueHost,
  lineMode: 'multi',
  contentTestId,
  scrollerTestId,
})

useLinkTitles(editorView, { readonly: () => readonly })

const { focused, focusHandlers } = useEditorFocus(editorView)
watch(focused, (focused) => {
  if (!focused && !editorView.state.selection.main.empty) {
    editorView.dispatch({
      selection: {
        anchor: editorView.state.selection.main.from,
        head: editorView.state.selection.main.from,
      },
    })
  }
})
const editing = computed(() => !readonly && focused.value)

const formatting = useMarkdownFormatting(editorView)
const { actions, formatBindings } = useFormatActions({
  formatting,
  readonly,
  editing,
  uploadImage: selectAndUpload,
})
setExtraExtensions([formatBindings])

editorReadyCallback(editorView)

const blockType = computed({
  get: () => formatting.blockType.value ?? 'Unknown',
  set: (value) => formatting.blockType.set(value as BlockType),
})
const blockTypeDropdown = useBlockTypeDropdown({ blockType, actions })

defineExpose({
  editorView,
})
</script>

<template>
  <div class="MarkdownEditorRoot" @dragover.prevent>
    <div v-if="toolbar" class="toolbar" @pointerdown.prevent>
      <ActionButton action="panel.fullscreen" />
      <SelectionDropdown v-if="blockTypeDropdown" v-bind="blockTypeDropdown" />
      <ActionButton action="documentationEditor.italic" />
      <ActionButton action="documentationEditor.bold" />
      <ActionButton action="documentationEditor.link" />
      <ActionButton action="documentationEditor.code" />
      <ActionButton action="documentationEditor.image" />
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
  isolation: isolate;
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
  /* Below popovers from the `belowToolbar` slot. */
  z-index: -1;
  min-height: 0;

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
    text-indent: -0.4em;
    /*noinspection CssUnresolvedCustomProperty*/
    margin-left: calc(var(--cm-list-depth) * 0.57em + 1.1em);
  }

  :global(.cm-OrderedList-item) {
    list-style-type: decimal;
    list-style-position: inside;
    /*noinspection CssUnresolvedCustomProperty*/
    margin-left: calc(var(--cm-list-depth) * 0.85em);
  }
}
</style>
