<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { WidgetInputIsSpecificMethodCall } from '@/components/GraphEditor/widgets/WidgetFunction.vue'
import { TextLanguage } from '@/components/GraphEditor/widgets/WidgetText.vue'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import { Ast } from '@/util/ast'
import { ArgumentApplicationKey } from '@/util/callTree'
import { ProjectPath } from '@/util/projectPath'
import { type Identifier, type QualifiedName } from '@/util/qualifiedName'
import { bracketMatching } from '@codemirror/language'
import * as objects from 'enso-common/src/utilities/data/object'
import { computed } from 'vue'

const { input } = defineProps(widgetProps(widgetDefinition))

const tableExprExtension = [bracketMatching()]

const innerInput = computed(() => {
  if (!input[ArgumentApplicationKey]) {
    console.warn('Unreachable: Widget definition matched, but no argument application was found.')
    return input
  }
  return objects.merge(input, {
    [ArgumentApplicationKey]: input[ArgumentApplicationKey].withArgumentMetadata({
      [TextLanguage]: tableExprExtension,
    }),
  })
})
</script>

<script lang="ts">
const isExprMethodCall = WidgetInputIsSpecificMethodCall({
  module: ProjectPath.create('Standard.Table' as QualifiedName, 'Expression' as QualifiedName),
  definedOnType: ProjectPath.create(
    'Standard.Table' as QualifiedName,
    'Expression' as QualifiedName,
  ),
  name: 'expr' as Identifier,
})

const isValidExprMethodCall = (
  input: WidgetInput,
): input is WidgetInput & { value: Ast.App & { argument: Ast.TextLiteral } } => {
  return (
    isExprMethodCall(input) &&
    input.value instanceof Ast.App &&
    !(input.value.function instanceof Ast.App) &&
    input.value.argument instanceof Ast.TextLiteral
  )
}

export const widgetDefinition = defineWidget(
  isValidExprMethodCall,
  {
    priority: 999,
    score: () => Score.Perfect,
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetTableExpr">
    <NodeWidget :input="innerInput" />
  </div>
</template>
