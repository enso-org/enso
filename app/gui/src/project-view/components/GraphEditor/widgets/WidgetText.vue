<script setup lang="ts">
import CodeMirrorInlineRoot from '@/components/CodeMirrorInlineRoot.vue'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { targetIsOutside } from '@/util/autoBlur'
import { selectOnMouseFocus, useCodeMirror, useStringSync } from '@/util/codemirror'
import { highlightStyle } from '@/util/codemirror/highlight'
import { type Extension } from '@codemirror/state'
import { computed, ref, useCssModule, useTemplateRef, watch, type ComponentInstance } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const widgetRoot = ref<HTMLElement>()

const textContents = computed(() =>
  props.input.value instanceof Ast.TextLiteral ? props.input.value.rawTextContent : '',
)

/** Widget Input as Text Literal; undefined if there's no value, or the value is not a Text literal. */
const inputTextLiteral = computed((): Ast.TextLiteral | undefined => {
  if (props.input.value instanceof Ast.TextLiteral) return props.input.value
  const valueStr = WidgetInput.valueRepr(props.input)
  if (valueStr == null) return undefined
  return Ast.TextLiteral.tryParse(valueStr)
})

const placeholder = computed(() =>
  WidgetInput.isPlaceholder(props.input) ? (inputTextLiteral.value?.rawTextContent ?? '') : '',
)

const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorInlineRoot>>('editorRoot')

const { syncExt, connectSync } = useStringSync()
const { editorView, setExtraExtensions } = useCodeMirror(editorRoot, {
  content: textContents.value,
  placeholder,
  extensions: [syncExt, selectOnMouseFocus, highlightStyle(useCssModule())],
  readonly: false,
  contentTestId: 'widget-text-content',
  singleLine: true,
})
watch(() => props.input[TextLanguage], setExtraExtensions, { immediate: true })

const { getText, setText, onTextEdited } = connectSync(editorView)
watch(textContents, setText)
onTextEdited((text) => editing.edit(makeLiteralFromUserInput(text)))

const previousValue = ref<string>()

const editing = WidgetEditHandler.New('WidgetText', props.input, {
  start() {
    previousValue.value = textContents.value
  },
  cancel() {
    if (getText() !== textContents.value) setText(textContents.value)
    blurEditor()
  },
  pointerdown(event) {
    if (targetIsOutside(event, widgetRoot.value)) {
      accepted()
    }
    return false
  },
  end() {
    blurEditor()
  },
})

function blurEditor() {
  editorView.contentDOM.blur()
}

function focusEditor() {
  editorView.dispatch({ selection: { anchor: 0, head: editorView.state.doc.length } })
  editorView.focus()
}

function accepted() {
  editing.end()
  const text = getText()
  if (props.input.value instanceof Ast.TextLiteral) {
    if (previousValue.value === text) return
    const edit = graph.startEdit()
    const value = edit.getVersion(props.input.value)
    if (value.rawTextContent === text) return
    value.setRawTextContent(text)
    props.onUpdate({ edit, directInteraction: true })
  } else {
    let value: Ast.Owned<Ast.MutableTextLiteral>
    if (inputTextLiteral.value) {
      value = Ast.copyIntoNewModule(inputTextLiteral.value)
      value.setRawTextContent(text)
    } else {
      value = Ast.TextLiteral.new(text)
    }
    props.onUpdate({
      portUpdate: {
        value,
        origin: props.input.portId,
      },
      directInteraction: true,
    })
  }
}

function makeLiteralFromUserInput(value: string): Ast.Owned<Ast.MutableTextLiteral> {
  if (props.input.value instanceof Ast.TextLiteral) {
    const literal = Ast.copyIntoNewModule(props.input.value)
    literal.setRawTextContent(value)
    return literal
  } else {
    return Ast.TextLiteral.new(value)
  }
}

const openToken = computed(() => inputTextLiteral.value?.open ?? emptyTextLiteral.value.open)
const closeToken = computed(() => inputTextLiteral.value?.close ?? openToken.value)
</script>

<script lang="ts">
export const TextLanguage: unique symbol = Symbol.for('WidgetInput:TextLanguage')
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [TextLanguage]?: Extension
  }
}

// Computed used intentionally to delay computation until wasm package is loaded.
const emptyTextLiteral = computed(() => Ast.TextLiteral.new(''))

export const widgetDefinition = defineWidget(
  WidgetInput.placeholderOrAstMatcher(Ast.TextLiteral),
  {
    priority: 1001,
    score: (props) => {
      if (props.input.value instanceof Ast.TextLiteral) return Score.Perfect
      if (props.input.dynamicConfig?.kind === 'Text_Input') return Score.Perfect
      const type = props.input.expectedType
      if (type === 'Standard.Base.Data.Text.Text') return Score.Good
      return Score.Mismatch
    },
  },
  import.meta.hot,
)
</script>

<template>
  <label
    ref="widgetRoot"
    class="WidgetText widgetRounded"
    @pointerdown.stop.prevent="focusEditor"
    @click.stop
  >
    <NodeWidget v-if="openToken" :input="WidgetInput.FromAst(openToken)" />
    <!-- Do not finish edit on blur here!

    It is tempting, but it breaks the cooperation with possible drop-down widget. Blur may be done on
    pointerdown, and if it would end the interaction, the drop down would also be hidden, making 
    any `click` event on it impossible.
    -->
    <CodeMirrorInlineRoot
      ref="editorRoot"
      @keydown.enter.stop="accepted"
      @keydown.tab.stop="accepted"
      @focusin="editing.start()"
    />
    <NodeWidget v-if="closeToken" :input="WidgetInput.FromAst(closeToken)" />
  </label>
</template>

<style scoped>
.WidgetText {
  display: inline-flex;
  background: var(--color-widget);
  border-radius: var(--radius-full);
  user-select: none;
  justify-content: center;
  align-items: center;
  min-width: var(--node-port-height);

  &:has(> :focus) {
    outline: none;
    background: var(--color-widget-focus);
  }

  &:deep(::selection) {
    background: var(--color-widget-selection);
  }
}

.selected .WidgetText {
  background: var(--color-widget-unfocus);
  &:has(> :focus) {
    outline: none;
    background: var(--color-widget-focus);
  }
}

:deep(.cm-scroller) {
  font-weight: 800;
}

.GraphNode:not(.selected) .WidgetText :deep(.cm-content) * {
  color: inherit;
}
</style>

<!--suppress CssUnusedSymbol -->
<style module>
.keyword,
.moduleKeyword,
.modifier {
  color: #708;
}
.number {
  color: #164;
}
.string {
  color: #a11;
}
.escape {
  color: #e40;
}
.variableName,
.definition-variableName {
  color: #00f;
}
.lineComment,
.docComment {
  color: #940;
}
.invalid {
  color: #f00;
}
</style>
