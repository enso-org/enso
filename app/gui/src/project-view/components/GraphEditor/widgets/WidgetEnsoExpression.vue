<script setup lang="ts">
import { ensoSyntax } from '@/components/CodeEditor/ensoSyntax'
import CodeMirrorWidgetBase from '@/components/GraphEditor/CodeMirrorWidgetBase.vue'
import {
  defineWidget,
  HandledUpdate,
  Score,
  WidgetInput,
  widgetProps,
} from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { defaultHighlightStyle, syntaxHighlighting } from '@codemirror/language'
import { computed, ref, useTemplateRef } from 'vue'
import { BodyBlock, MutableModule } from 'ydoc-shared/ast'

const props = defineProps(widgetProps(widgetDefinition))

const astCode = computed(() => {
  if (WidgetInput.isPlaceholder(props.input)) return '' // We display the value as placeholder.
  return WidgetInput.valueRepr(props.input) ?? ''
})

function acceptValue(value: string): HandledUpdate {
  return props.onUpdate({
    portUpdate: {
      value: Ast.parseExpression(value),
      origin: props.input.portId,
    },
    directInteraction: true,
  })
}

const placeholder = computed(() => {
  const input = props.input
  return WidgetInput.isPlaceholder(input) ? (input.value ?? '') : ''
})

const moduleRoot = ref(BodyBlock.new([], MutableModule.Transient()))
const extensions = [
  syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
  ensoSyntax(moduleRoot),
]

const cmWidget = useTemplateRef('cmWidget')
</script>

<script lang="ts">
export const EnsoExpression: unique symbol = Symbol.for('WidgetInput:EnsoExpression')
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [EnsoExpression]?: {
      weakMatch?: boolean
    }
  }
}

export const widgetDefinition = defineWidget(
  EnsoExpression,
  {
    priority: 150,
    score: (props) => (props.input[EnsoExpression].weakMatch === true ? Score.Weak : Score.Perfect),
  },
  import.meta.hot,
)
</script>

<template>
  <div
    class="WidgetEnsoExpression widgetRounded widgetPill"
    @click.stop="cmWidget?.focusAndSelect()"
  >
    <CodeMirrorWidgetBase
      ref="cmWidget"
      v-model="astCode"
      :widgetTypeId="widgetTypeId"
      :input="input"
      :placeholder="placeholder"
      :extensions="extensions"
      lineMode="single"
      :onAccepted="acceptValue"
    />
  </div>
</template>

<style scoped>
.WidgetEnsoExpression {
  display: inline-flex;
  justify-content: center;
  align-items: center;
}
</style>
