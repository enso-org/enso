<script setup lang="ts">
import {
  defineWidget,
  Score,
  WidgetInput,
  widgetProps,
  type HandledUpdate,
} from '$/providers/openedProjects/widgetRegistry'
import { ensoSyntax } from '@/components/CodeEditor/ensoSyntax'
import CodeMirrorWidgetBase from '@/components/GraphEditor/CodeMirrorWidgetBase.vue'
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
  const preprocess = props.input[EnsoExpression].preprocess
  const preprocessed = preprocess ? preprocess(value) : value
  return props.updateCallback({
    portUpdate: {
      value: Ast.parseExpression(preprocessed),
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
declare module '$/providers/openedProjects/widgetRegistry' {
  export interface WidgetInput {
    [EnsoExpression]?: {
      weakMatch?: boolean | undefined
      preprocess?: ((code: string) => string) | undefined
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
    class="WidgetEnsoExpression widgetExpanded widgetRounded widgetPill"
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
      :syncAfterAccept="true"
    />
  </div>
</template>
