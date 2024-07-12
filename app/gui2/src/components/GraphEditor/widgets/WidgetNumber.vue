<script setup lang="ts">
import NumericInputWidget from '@/components/widgets/NumericInputWidget.vue'
import { Score, WidgetInput, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { Ast } from '@/util/ast'
import { targetIsOutside } from '@/util/autoBlur'
import { unrefElement } from '@vueuse/core'
import { computed, ref, type ComponentInstance } from 'vue'

const props = defineProps(widgetProps(widgetDefinition))
const inputComponent = ref<ComponentInstance<typeof NumericInputWidget>>()

function setValue(value: string | undefined) {
  props.onUpdate({
    portUpdate: { value, origin: props.input.portId },
  })
}

const value = computed<number | undefined>(() => {
  const inputValue = WidgetInput.valueRepr(props.input)
  if (inputValue == null) return undefined
  const inputNumber = parseFloat(inputValue)
  if (Number.isNaN(inputNumber)) return undefined
  return inputNumber
})

const placeholder = computed<string | undefined>(() =>
  value.value == null ? WidgetInput.valueRepr(props.input) : undefined,
)

const limits = computed(() => {
  const config = props.input.dynamicConfig
  if (config?.kind === 'Numeric_Input' && config?.minimum != null && config?.maximum != null) {
    return { min: config.minimum, max: config.maximum }
  } else {
    return undefined
  }
})

const editHandler = WidgetEditHandler.New('WidgetNumber', props.input, {
  cancel: () => inputComponent.value?.cancel(),
  start: () => inputComponent.value?.focus(),
  pointerdown(event) {
    if (targetIsOutside(event, unrefElement(inputComponent))) editHandler.end()
    return false
  },
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
  <NumericInputWidget
    ref="inputComponent"
    class="WidgetNumber r-24"
    :limits="limits"
    :placeholder="placeholder"
    :modelValue="value"
    @update:modelValue="setValue"
    @click.stop
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
