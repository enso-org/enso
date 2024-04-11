<script setup lang="ts">
import NumericInputWidget from '@/components/widgets/NumericInputWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { Ast } from '@/util/ast'
import { computed, ref, type ComponentInstance } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const inputComponent = ref<ComponentInstance<typeof NumericInputWidget>>()
const value = computed({
  get() {
    const valueStr = WidgetInput.valueRepr(props.input)
    return valueStr ? parseFloat(valueStr) : 0
  },
  set(value) {
    props.onUpdate({
      portUpdate: { value: value.toString(), origin: props.input.portId },
    })
  },
})

const limits = computed(() => {
  const config = props.input.dynamicConfig
  if (config?.kind === 'Numeric_Input' && config?.minimum != null && config?.maximum != null) {
    return { min: config.minimum, max: config.maximum }
  } else {
    return undefined
  }
})

const editHandler = WidgetEditHandler.New(props.input, {
  cancel: () => inputComponent.value?.cancel(),
  start: () => inputComponent.value?.focus(),
  end: () => inputComponent.value?.blur(),
})
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInput.isAstOrPlaceholder,
  {
    priority: 1001,
    score: (props) => {
      if (
        props.input.value instanceof Ast.NumericLiteral ||
        (props.input.value instanceof Ast.NegationApp &&
          props.input.value.argument instanceof Ast.NumericLiteral)
      )
        return Score.Perfect
      const type = props.input.expectedType
      if (
        type === 'Standard.Base.Data.Numbers.Number' ||
        type === 'Standard.Base.Data.Numbers.Integer' ||
        type === 'Standard.Base.Data.Numbers.Float'
      )
        return Score.Good
      return Score.Mismatch
    },
  },
  import.meta.hot,
)
</script>

<template>
  <!-- See comment in GraphNode next to dragPointer definition about stopping pointerdown and pointerup -->
  <NumericInputWidget
    ref="inputComponent"
    v-model="value"
    class="WidgetNumber r-24"
    :limits="limits"
    @pointerdown.stop
    @pointerup.stop
    @focus="editHandler.start()"
    @blur="editHandler.end()"
    @input="editHandler.edit($event)"
  />
</template>

<style scoped>
.WidgetNumber {
  display: inline-block;
  vertical-align: middle;
}
</style>
