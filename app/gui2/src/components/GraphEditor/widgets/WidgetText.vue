<script setup lang="ts">
import EnsoTextInputWidget from '@/components/widgets/EnsoTextInputWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { useGraphStore } from '@/stores/graph'
import { Ast } from '@/util/ast'
import type { TokenId } from '@/util/ast/abstract'
import { asNot } from '@/util/data/types'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const value = computed({
  get() {
    const valueStr = WidgetInput.valueRepr(props.input)
    return valueStr ?? ''
  },
  set(value) {
    props.onUpdate({
      edit: graph.astModule.edit(),
      portUpdate: { value: value.toString(), origin: asNot<TokenId>(props.input.portId) },
    })
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
    if (type === 'Standard.Base.Data.Text') return Score.Good
    return Score.Mismatch
  },
})
</script>

<template>
  <EnsoTextInputWidget v-model="value" class="WidgetText r-24" />
</template>

<style scoped>
.WidgetText {
  display: inline-block;
  vertical-align: middle;
}
</style>
