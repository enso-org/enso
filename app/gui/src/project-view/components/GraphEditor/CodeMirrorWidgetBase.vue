<script setup lang="ts">
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import { defineWidget, Score, WidgetInput } from '@/providers/widgetRegistry'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { Ast } from '@/util/ast'
import { targetIsOutside } from '@/util/autoBlur'
import { selectOnMouseFocus, useCodeMirror, useStringSync } from '@/util/codemirror'
import { highlightStyle } from '@/util/codemirror/highlight'
import { Extension, SelectionRange } from '@codemirror/state'
import { ComponentInstance, ref, useTemplateRef, watch, watchEffect } from 'vue'

const props = defineProps<{
  input: WidgetInput
  placeholder?: string | undefined
  /**
   * Additional extensions to provide to codemirror editor. Usually useful for language definition.
   */
  extensions?: Extension[]
  /**
   * If provided, the element with class `cm-content` will also have the given `data-testid`.
   * Warning: Not reactive - Set only once during setup.
   */
  contentTestId?: string
  transformUserInput?: (value: string) => Ast.Owned<Ast.MutableTextLiteral> | string
  /**
   * Editor line mode.
   * @default 'single'
   */
  lineMode?: 'single' | 'multi' | 'auto'
}>()

const model = defineModel<string>({ default: '' })
const emit = defineEmits<{
  textEdited: [text: string]
  userAction: [text: string, selection: SelectionRange]
}>()

const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorRoot>>('editorRoot')

function onAccepted(newValue: string) {
  model.value = newValue
}

const { syncExt, connectSync } = useStringSync()
const { editorView, setExtraExtensions } = useCodeMirror(editorRoot, {
  content: model.value,
  placeholder: () => props.placeholder ?? ' ',
  extensions: [syncExt],
  readonly: false,
  contentTestId: props.contentTestId,
  lineMode: () => props.lineMode ?? 'single',
})
watchEffect(() =>
  setExtraExtensions([
    highlightStyle(editorRoot.value?.highlightClasses ?? {}),
    ...(props.lineMode !== 'multi' ? [selectOnMouseFocus] : []),
    ...(props.extensions ?? []),
  ]),
)

const { getText, setText, onTextEdited, onUserAction } = connectSync(editorView)
watch(model, (text) => setText(text))
onTextEdited((text) => {
  editing.edit(props.transformUserInput?.(text) ?? text)
  emit('textEdited', text)
})
onUserAction((text, selection) => emit('userAction', text, selection))

const previousValue = ref<string>()
const editing = WidgetEditHandler.New(props, {
  start() {
    previousValue.value = model.value
  },
  cancel() {
    if (getText() !== model.value) setText(model.value)
    blurEditor()
  },
  pointerdown(event) {
    if (targetIsOutside(event, editorRoot.value?.$el.value)) {
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
  const text = getText()
  if (previousValue.value === text) {
    editing.cancel()
  }
  editing.end()
  onAccepted(text)
}

function onEnter(event: KeyboardEvent) {
  if (props.lineMode == 'single' || !event.shiftKey) {
    event.stopPropagation()
    accepted()
  }
}

defineExpose({
  focusEditor,
  blurEditor,
  setText,
})
</script>

<script lang="ts">
export const EnsoExpression: unique symbol = Symbol.for('WidgetInput:EnsoExpression')
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [EnsoExpression]?: {
      validateInput?: (ast: Ast.Expression) => boolean
    }
  }
}

export const widgetDefinition = defineWidget(
  EnsoExpression,
  {
    priority: 1002,
    score: Score.Good,
  },
  import.meta.hot,
)
</script>

<template>
  <CodeMirrorRoot
    ref="editorRoot"
    class="CodeMirrorRoot widgetApplyMargin"
    @focusin="editing.start()"
    @keydown.enter="onEnter"
    @keydown.tab.stop.capture="accepted"
    @keydown.up.stop
    @keydown.down.stop
    @click.stop
  />
</template>
<style scoped>
.CodeMirrorRoot {
  &:deep(::selection) {
    background: var(--color-widget-selection);
  }
}
</style>
