<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import AutoSizedInput from '@/components/widgets/AutoSizedInput.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
import { MutableModule } from '@/util/ast/abstract'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()

const inputTextLiteral = computed((): Ast.TextLiteral | undefined => {
  if (props.input.value instanceof Ast.TextLiteral) return props.input.value
  const valueStr = WidgetInput.valueRepr(props.input)
  const parsed = valueStr != null ? Ast.parse(valueStr) : undefined
  if (parsed instanceof Ast.TextLiteral) return parsed
  return undefined
})

function makeNewLiteral(value: string) {
  return Ast.TextLiteral.new(value, MutableModule.Transient())
}

const emptyTextLiteral = makeNewLiteral('')
const shownLiteral = computed(() => inputTextLiteral.value ?? emptyTextLiteral)
const closeToken = computed(() => shownLiteral.value.close ?? shownLiteral.value.open)

const textContents = computed({
  get() {
    return shownLiteral.value.rawTextContent
  },
  set(value) {
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
  },
})
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
    <AutoSizedInput v-model.lazy="textContents" />
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
