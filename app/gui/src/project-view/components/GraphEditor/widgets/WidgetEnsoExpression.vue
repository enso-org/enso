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
import { computed, ref } from 'vue'
import { BodyBlock, MutableModule } from 'ydoc-shared/ast'

const props = defineProps(widgetProps(widgetDefinition))

const astCode = computed(() => WidgetInput.valueRepr(props.input) ?? '')

function acceptValue(value: string): HandledUpdate {
  return props.onUpdate({
    portUpdate: {
      value: Ast.parseExpression(value),
      origin: props.input.portId,
    },
    directInteraction: true,
  })
}

const moduleRoot = ref(BodyBlock.new([], MutableModule.Transient()))
const extensions = [
  syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
  ensoSyntax(moduleRoot),
]
</script>

<script lang="ts">
export const EnsoExpression: unique symbol = Symbol.for('WidgetInput:EnsoExpression')
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [EnsoExpression]?: object
  }
}

export const widgetDefinition = defineWidget(
  EnsoExpression,
  {
    priority: 1002,
    score: Score.Perfect,
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetEnsoExpression widgetRounded widgetPill">
    <CodeMirrorWidgetBase
      v-model="astCode"
      :widgetTypeId="widgetTypeId"
      :input="input"
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
