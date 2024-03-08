<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import AutoSizedInput from '@/components/widgets/AutoSizedInput.vue'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { MutableModule } from '@/util/ast/abstract'
import { computed, ref } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const input = ref<HTMLElement>()

function editingStarted() {
  interaction.setCurrent(editing)
  props.input.editing?.onStarted(editing)
}

function editingEnded() {
  interaction.end(editing)
  props.input.editing?.onFinished()
}

function accepted(value: string) {
  console.log('Accepted in widget text', value)
  if (interaction.isActive(editing)) {
    if (props.input.value instanceof Ast.TextLiteral) {
      const edit = graph.startEdit()
      edit.getVersion(props.input.value).setRawTextContent(value)
      props.onUpdate({ edit })
    } else {
      props.onUpdate({
        portUpdate: {
          value: makeNewLiteral(value).code(),
          origin: props.input.portId,
        },
      })
    }
  }
}

const inputTextLiteral = computed((): Ast.TextLiteral | undefined => {
  if (props.input.value instanceof Ast.TextLiteral) return props.input.value
  const valueStr = WidgetInput.valueRepr(props.input)
  if (valueStr == null) return undefined
  return Ast.TextLiteral.tryParse(valueStr)
})

function makeNewLiteral(value: string) {
  return Ast.TextLiteral.new(value, MutableModule.Transient())
}

function makeLiteralFromValue(value: string) {
  if (props.input.value instanceof Ast.TextLiteral) {
    const edit = graph.startEdit()
    const literal = edit.getVersion(props.input.value)
    literal.setRawTextContent(value)
    return literal.code()
  } else {
    return makeNewLiteral(value).code()
  }
}

const emptyTextLiteral = makeNewLiteral('')
const shownLiteral = computed(() => inputTextLiteral.value ?? emptyTextLiteral)
const closeToken = computed(() => shownLiteral.value.close ?? shownLiteral.value.open)

const textContents = computed(() => shownLiteral.value.rawTextContent)
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(WidgetInput.isAstOrPlaceholder, {
  priority: 1001,
  score: (props) => {
    if (props.input.value instanceof Ast.TextLiteral) return Score.Perfect
    if (props.input.dynamicConfig?.kind === 'Text_Input') return Score.Perfect
    const type = props.input.expectedType
    if (type === 'Standard.Base.Data.Text.Text') return Score.Good
    return Score.Mismatch
  },
})
</script>

<template>
  <label class="WidgetText r-24" @pointerdown.stop>
    <NodeWidget v-if="shownLiteral.open" :input="WidgetInput.FromAst(shownLiteral.open)" />
    <AutoSizedInput
      ref="input"
      v-model.lazy.interaction="textContents"
      @pointerdown.stop
      @pointerup.stop
      @click.stop
      @focusout="props.input.editing?.onFinished()"
      @interactionStarted="props.input.editing?.onStarted($event)"
      @input="props.input.editing?.onEdited(makeLiteralFromValue($event ?? ''))"
      @changed="accepted"
    />
    <NodeWidget v-if="closeToken" :input="WidgetInput.FromAst(closeToken)" />
  </label>
</template>

<style scoped>
.WidgetText {
  display: inline-flex;
  background: var(--color-widget);
  border-radius: var(--radius-full);
  position: relative;
  user-select: none;
  border-radius: var(--radius-full);
  padding: 0px 4px;
  min-width: 24px;
  justify-content: center;
  align-items: center;

  &:has(> .AutoSizedInput:focus) {
    outline: none;
    background: var(--color-widget-focus);
  }
}
</style>
