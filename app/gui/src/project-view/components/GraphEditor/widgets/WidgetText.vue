<script setup lang="ts">
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { targetIsOutside } from '@/util/autoBlur'
import { selectOnMouseFocus, useCodeMirror, useStringSync } from '@/util/codemirror'
import { highlightStyle } from '@/util/codemirror/highlight'
import { computed, ref, useTemplateRef, watch, watchEffect, type ComponentInstance } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const widgetRoot = ref<HTMLElement>()

const textContents = computed(() =>
  props.input.value instanceof Ast.TextLiteral ? props.input.value.rawTextContent : '',
)

const syntaxLanguage = computed(() =>
  props.input.dynamicConfig?.kind === 'Text_Input' ? props.input.dynamicConfig.syntax : undefined,
)

/** Widget Input as Text Literal; undefined if there's no value, or the value is not a Text literal. */
const inputTextLiteral = computed((): Ast.TextLiteral | undefined => {
  if (props.input.value instanceof Ast.TextLiteral) return props.input.value
  const valueStr = WidgetInput.valueRepr(props.input)
  if (valueStr == null) return undefined
  return Ast.TextLiteral.tryParse(valueStr)
})
const openToken = computed(() => inputTextLiteral.value?.open ?? emptyTextLiteral.value.open)
const closeToken = computed(() =>
  isBlock.value ? undefined : (inputTextLiteral.value?.close ?? openToken.value),
)
const isBlock = computed<boolean>(() => !!inputTextLiteral.value?.isBlock)
const editedTextIsMultiline = ref(isTextMultiline(textContents.value))
const isMultiline = computed<boolean>(() => isBlock.value || editedTextIsMultiline.value)

const placeholder = computed(() =>
  WidgetInput.isPlaceholder(props.input) ? (inputTextLiteral.value?.rawTextContent ?? '') : '',
)

const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorRoot>>('editorRoot')

const languageExtension = computed(() => {
  switch (syntaxLanguage.value) {
    case 'enso-table-expression':
      // TODO (#12304)
      return
  }
  return undefined
})

const { syncExt, connectSync } = useStringSync()
const { editorView, setExtraExtensions } = useCodeMirror(editorRoot, {
  content: textContents.value,
  placeholder,
  extensions: [syncExt],
  readonly: false,
  contentTestId: 'widget-text-content',
  lineMode: computed(() => (isMultiline.value ? 'multi' : 'auto')),
})
watchEffect(() =>
  setExtraExtensions([
    highlightStyle(editorRoot.value?.highlightClasses ?? {}),
    ...[languageExtension.value ?? []],
    ...(isMultiline.value ? [] : [selectOnMouseFocus]),
  ]),
)

function isTextMultiline(text: string) {
  return !!text.match(/[\r\n]/)
}

const { getText, setText, onTextEdited } = connectSync(editorView)
watch(textContents, (text) => setText(text))
onTextEdited((text) => editing.edit(makeLiteralFromUserInput(text)))
onTextEdited((text) => (editedTextIsMultiline.value = isTextMultiline(text)))

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

function onEnter(event: KeyboardEvent) {
  if (!event.shiftKey) {
    event.stopPropagation()
    accepted()
  }
}
</script>

<script lang="ts">
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
    :class="{ singleLine: !isMultiline }"
    @pointerdown.stop.prevent="focusEditor"
    @click.stop
  >
    <NodeWidget v-if="openToken" :input="WidgetInput.FromAst(openToken)" class="delimiter open" />
    <!-- Do not finish edit on blur here!

    It is tempting, but it breaks the cooperation with possible drop-down widget. Blur may be done on
    pointerdown, and if it would end the interaction, the drop down would also be hidden, making 
    any `click` event on it impossible.
    -->
    <CodeMirrorRoot
      ref="editorRoot"
      :data-syntax-language="syntaxLanguage"
      @focusin="editing.start()"
      @keydown.enter.capture="onEnter"
      @keydown.tab.stop.capture="accepted"
      @keydown.up.stop
      @keydown.down.stop
    />
    <NodeWidget
      v-if="closeToken"
      :input="WidgetInput.FromAst(closeToken)"
      class="delimiter close"
    />
  </label>
</template>

<style scoped>
.WidgetText {
  display: inline-flex;
  background: var(--color-widget);
  user-select: none;
  justify-content: center;
  align-items: center;
  min-width: var(--node-port-height);
  border-radius: var(--radius-default);

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

.singleLine :deep(.cm-scroller) {
  font-weight: 800;
}

/**
 * In multiline mode the widget is still sized to content (unless max-height is exceeded), but the
 * content is padded to be slightly larger than its scroller so that the scrollbar shows.
 */
.WidgetText:not(.singleLine) {
  & :deep(.cm-scroller) {
    min-height: 2.5em;
    max-height: 20em;
  }
  & .delimiter {
    font-size: 1.4em;
    &.open {
      align-self: flex-start;
    }
    &.close {
      align-self: flex-end;
    }
  }
}

.GraphNode:not(.selected) .WidgetText :deep(.cm-content) * {
  color: inherit;
}
</style>
