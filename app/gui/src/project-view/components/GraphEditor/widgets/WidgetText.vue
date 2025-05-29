<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import {
  defineWidget,
  HandledUpdate,
  Score,
  WidgetInput,
  widgetProps,
} from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { languageExtension } from '@/util/codemirror/language'
import { computed, ref, useTemplateRef } from 'vue'
import { Ok } from 'ydoc-shared/util/data/result'
import CodeMirrorWidgetBase from '../CodeMirrorWidgetBase.vue'

const baseEditor = useTemplateRef('baseEditor')
const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()

function focusEditor() {
  baseEditor.value?.focusEditor()
}

const textContents = computed(() =>
  props.input.value instanceof Ast.TextLiteral ? props.input.value.rawTextContent : '',
)
function acceptValue(text: string): HandledUpdate {
  if (props.input.value instanceof Ast.TextLiteral) {
    const edit = graph.startEdit()
    const value = edit.getVersion(props.input.value)
    if (value.rawTextContent === text) return Ok()
    value.setRawTextContent(text)
    return props.onUpdate({ edit, directInteraction: true })
  } else {
    let value: Ast.Owned<Ast.MutableTextLiteral>
    if (inputTextLiteral.value) {
      value = Ast.copyIntoNewModule(inputTextLiteral.value)
      value.setRawTextContent(text)
    } else {
      value = Ast.TextLiteral.new(text)
    }
    return props.onUpdate({
      portUpdate: {
        value,
        origin: props.input.portId,
      },
      directInteraction: true,
    })
  }
}

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

const languageExt = computed(() => languageExtension(syntaxLanguage.value))
const extensions = computed(() => (languageExt.value ? [languageExt.value] : []))

function isTextMultiline(text: string) {
  return !!text.match(/[\r\n]/)
}

function onTextEdited(text: string) {
  editedTextIsMultiline.value = isTextMultiline(text)
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
    class="WidgetText widgetRounded widgetPill"
    :class="{ singleLine: !isMultiline }"
    @pointerdown.stop.prevent="focusEditor"
    @click.stop
  >
    <NodeWidget v-if="openToken" :input="WidgetInput.FromAst(openToken)" class="delimiter open" />
    <CodeMirrorWidgetBase
      ref="baseEditor"
      v-model="textContents"
      contentTestId="widget-text-content"
      :placeholder="placeholder"
      :lineMode="isMultiline ? 'autoMulti' : 'auto'"
      :extensions="extensions"
      :widgetTypeId="widgetTypeId"
      :input="input"
      :transformUserInput="makeLiteralFromUserInput"
      :onAccepted="acceptValue"
      @textEdited="onTextEdited"
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
  justify-content: center;
  align-items: center;
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
  & :deep(.cm-line) {
    padding: 0;
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
</style>
