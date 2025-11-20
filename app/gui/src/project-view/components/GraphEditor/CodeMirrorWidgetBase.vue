<script setup lang="ts">
import type {
  HandledUpdate,
  WidgetInput,
  WidgetTypeId,
} from '$/providers/openedProjects/widgetRegistry'
import { WidgetEditHandler } from '$/providers/openedProjects/widgetRegistry/editHandler'
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import VueHostRender, { VueHostInstance } from '@/components/VueHostRender.vue'
import { Ast } from '@/util/ast'
import { targetIsOutside } from '@/util/autoBlur'
import { selectOnMouseFocus, useCodeMirror, useStringSync } from '@/util/codemirror'
import { highlightStyle } from '@/util/codemirror/highlight'
import { useToast } from '@/util/toast'
import { SelectionRange, type Extension } from '@codemirror/state'
import { Ok } from 'enso-common/src/utilities/data/result'
import { ref, useTemplateRef, watch, type ComponentInstance } from 'vue'

const props = defineProps<{
  widgetTypeId: WidgetTypeId
  input: WidgetInput
  placeholder?: string | undefined
  /** Additional extensions to provide to codemirror editor. */
  extensions?: Extension
  /**
   * If provided, the element with class `cm-content` will also have the given `data-testid`.
   * Warning: Not reactive - Set only once during setup.
   */
  contentTestId?: string
  transformUserInput?: (value: string) => Ast.Owned<Ast.MutableTextLiteral> | string
  /** Editor line mode. Single-line mode will not allow entering newline characters. */
  lineMode: 'single' | 'multi' | 'auto' | 'autoMulti'
  syncAfterAccept?: boolean
  onAccepted?: (value: string) => HandledUpdate
}>()

const model = defineModel<string>({ default: '' })
const emit = defineEmits<{
  textEdited: [text: string]
  userAction: [text: string, selection: SelectionRange]
  blur: []
}>()

const editorRoot = useTemplateRef<ComponentInstance<typeof CodeMirrorRoot>>('editorRoot')

const { syncExt, getText, setText } = useStringSync({
  onTextEdited: (text) => {
    editing.value.edit(props.transformUserInput?.(text) ?? text)
    emit('textEdited', text)
  },
  onUserAction: (text, selection) => emit('userAction', text, selection),
})
const vueHost = new VueHostInstance()
const { editorView } = useCodeMirror(editorRoot, {
  placeholder: () => props.placeholder ?? ' ',
  extensions: [
    syncExt,
    () => (editorRoot.value ? highlightStyle(editorRoot.value.highlightClasses) : []),
    () =>
      props.lineMode !== 'multi' && props.lineMode !== 'autoMulti' ? [selectOnMouseFocus] : [],
    () => props.extensions ?? [],
  ],
  readonly: false,
  contentTestId: props.contentTestId,
  lineMode: () => props.lineMode ?? 'single',
  vueHost: () => vueHost,
})

watch(model, (text) => setText(editorView, text), { immediate: true })

const previousValue = ref<string>()
const editing = WidgetEditHandler.New(props, {
  start() {
    previousValue.value = model.value
  },
  cancel() {
    if (getText(editorView) !== model.value) setText(editorView, model.value)
    blurEditor()
  },
  pointerdown(event) {
    if (targetIsOutside(event, editorRoot.value?.$el)) {
      accepted()
    }
    return false
  },
  end() {
    if (props.syncAfterAccept && getText(editorView) !== model.value)
      setText(editorView, model.value)
    blurEditor()
  },
})

function blurEditor() {
  editorView.contentDOM.blur()
  emit('blur')
}

function focusAndSelect() {
  editorView.dispatch({ selection: { anchor: 0, head: editorView.state.doc.length } })
  editorView.focus()
}

const inputError = useToast.error()

async function accepted() {
  const text = getText(editorView)
  if (previousValue.value === text) {
    editing.value.end()
    return
  }
  const result = await handleAccept(text)
  if (result.ok) {
    editing.value.end()
  } else {
    inputError.reportError(result.error)
    editing.value.cancel()
  }
}

function handleAccept(newValue: string) {
  if (props.onAccepted) {
    return props.onAccepted(newValue)
  } else {
    model.value = newValue
    return Ok()
  }
}

function onEnter(event: KeyboardEvent) {
  if (props.lineMode == 'single' || !event.shiftKey) {
    event.stopPropagation()
    accepted()
  }
}

defineExpose({
  focusAndSelect,
})
</script>

<template>
  <CodeMirrorRoot
    ref="editorRoot"
    class="CodeMirrorWidgetBase widgetApplyPadding"
    @focusin="editing.start()"
    @keydown.enter="onEnter"
    @keydown.tab.stop.capture="accepted"
    @keydown.up.stop
    @keydown.down.stop
    @click.stop
    @wheel.stop.passive
  >
    <VueHostRender :host="vueHost" />
  </CodeMirrorRoot>
</template>

<style scoped>
/*noinspection CssUnusedSymbol*/
.CodeMirrorWidgetBase :deep(.cm-content) {
  caret-color: var(--color-node-text);
}
</style>
