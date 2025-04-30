<script setup lang="ts">
import { ensoSyntax } from '@/components/CodeEditor/ensoSyntax'
import CodeMirrorWidgetBase from '@/components/GraphEditor/CodeMirrorWidgetBase.vue'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { defaultHighlightStyle, syntaxHighlighting } from '@codemirror/language'
import { computed, ref } from 'vue'
import { BodyBlock, MutableModule } from 'ydoc-shared/ast'

const props = defineProps(widgetProps(widgetDefinition))

const astCode = computed({
  get: () => WidgetInput.valueRepr(props.input) ?? '',
  set: (value) => {
    const newAst = Ast.parseExpression(value)
    if (validateAst(newAst)) {
      props.onUpdate({
        portUpdate: {
          value: newAst,
          origin: props.input.portId,
        },
        directInteraction: true,
      })
    }
  },
})

function validateAst(ast: Ast.Expression | undefined): boolean {
  return ast != null && (props.input[EnsoExpression].validateInput?.(ast) ?? true)
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
    [EnsoExpression]?: {
      validateInput?: (ast: Ast.Expression) => boolean
    }
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
  <div class="WidgetEnsoExpression widgetRounded">
    <CodeMirrorWidgetBase v-model="astCode" :input="input" :extensions="extensions" />
  </div>
</template>

<style scoped>
.WidgetEnsoExpression {
  display: inline-flex;
  background: var(--color-widget);
  min-width: var(--node-port-height);
  border-radius: var(--radius-default);
  justify-content: center;
  align-items: center;
}
</style>
