<script setup lang="ts">
import EnsoTextInputWidget from '@/components/widgets/EnsoTextInputWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import type { TokenId } from '@/util/ast/abstract'
import { asNot } from '@/util/data/types'
import { computed } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const value = computed({
  get() {
    const valueStr = WidgetInput.valueRepr(props.input)
    return typeof valueStr === 'string' && Ast.parse(valueStr) instanceof Ast.TextLiteral
      ? valueStr
      : ''
  },
  set(value) {
    props.onUpdate({
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
    if (type === 'Standard.Base.Data.Text.Text') return Score.Good
    return Score.Mismatch
  },
})
</script>

<template>
  <!-- See comment in GraphNode next to dragPointer definition about stopping pointerdown -->
  <EnsoTextInputWidget v-model="value" class="WidgetText r-24" @pointerdown.stop />
</template>

<style scoped>
.WidgetText {
  display: inline-block;
  vertical-align: middle;
}
</style>
