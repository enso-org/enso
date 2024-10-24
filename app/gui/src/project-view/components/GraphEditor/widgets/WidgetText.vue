<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import AutoSizedInput from '@/components/widgets/AutoSizedInput.vue'
import { unrefElement } from '@/composables/events'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { MutableModule } from '@/util/ast/abstract'
import { targetIsOutside } from '@/util/autoBlur'
import { computed, ref, watch, type ComponentInstance } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const input = ref<ComponentInstance<typeof AutoSizedInput>>()
const widgetRoot = ref<HTMLElement>()

const editing = WidgetEditHandler.New('WidgetText', props.input, {
  cancel() {
    editedContents.value = textContents.value
    input.value?.blur()
  },
  pointerdown(event) {
    if (targetIsOutside(event, unrefElement(input))) {
      accepted()
    }
    return false
  },
  end() {
    input.value?.blur()
  },
})

function accepted() {
  editing.end()
  if (props.input.value instanceof Ast.TextLiteral) {
    const edit = graph.startEdit()
    edit.getVersion(props.input.value).setRawTextContent(editedContents.value)
    props.onUpdate({ edit })
  } else {
    props.onUpdate({
      portUpdate: {
        value: makeNewLiteral(editedContents.value),
        origin: props.input.portId,
      },
    })
  }
}

const inputTextLiteral = computed((): Ast.TextLiteral | undefined => {
  if (props.input.value instanceof Ast.TextLiteral) return props.input.value
  const valueStr = WidgetInput.valueRepr(props.input)
  if (valueStr == null) return undefined
  return Ast.TextLiteral.tryParse(valueStr)
})

function makeLiteralFromUserInput(value: string): Ast.Owned<Ast.MutableTextLiteral> {
  if (props.input.value instanceof Ast.TextLiteral) {
    const literal = MutableModule.Transient().copy(props.input.value)
    literal.setRawTextContent(value)
    return literal
  } else {
    return makeNewLiteral(value)
  }
}

const shownLiteral = computed(() => inputTextLiteral.value ?? emptyTextLiteral.value)
const closeToken = computed(() => shownLiteral.value.close ?? shownLiteral.value.open)

const textContents = computed(() => shownLiteral.value.rawTextContent)
const editedContents = ref(textContents.value)
watch(textContents, (value) => (editedContents.value = value))
</script>

<script lang="ts">
// Computed used intentionally to delay computation until wasm package is loaded.
const emptyTextLiteral = computed(() => makeNewLiteral(''))
function makeNewLiteral(value: string) {
  return Ast.TextLiteral.new(value, MutableModule.Transient())
}

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
  <label ref="widgetRoot" class="WidgetText widgetRounded">
    <NodeWidget v-if="shownLiteral.open" :input="WidgetInput.FromAst(shownLiteral.open)" />
    <!-- Do not finish edit on blur here!

    It is tempting, but it breaks the cooperation with possible drop-down widget. Blur may be done on
    pointerdown, and if it would end the interaction, the drop down would also be hidden, making 
    any `click` event on it impossible.
    -->
    <AutoSizedInput
      ref="input"
      v-model="editedContents"
      autoSelect
      @keydown.enter.stop="accepted"
      @keydown.tab.stop="accepted"
      @focusin="editing.start()"
      @input="editing.edit(makeLiteralFromUserInput($event ?? ''))"
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
  border-radius: var(--radius-full);
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
</style>
