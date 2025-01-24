<script setup lang="ts">
import NodeWidget from '@/components/GraphEditor/NodeWidget.vue'
import { FunctionName } from '@/components/GraphEditor/widgets/WidgetFunctionName.vue'
import { DisplayIcon } from '@/components/GraphEditor/widgets/WidgetIcon.vue'
import { defineWidget, Score, WidgetInput, widgetProps } from '@/providers/widgetRegistry'
import { DocumentationData } from '@/stores/suggestionDatabase/documentation'
import { Ast } from '@/util/ast'
import { type MethodPointer } from '@/util/methodPointer'
import { computed, Ref } from 'vue'
import ArgumentRow from './WidgetFunctionDef/ArgumentRow.vue'

const { input } = defineProps(widgetProps(widgetDefinition))

const funcIcon = computed(() => {
  return input[FunctionInfoKey]?.docsData.value?.iconName ?? 'enso_logo'
})

const funcNameInput = computed(() => {
  const nameAst = input.value.name
  const widgetInput = WidgetInput.FromAst(nameAst)
  widgetInput[DisplayIcon] = {
    icon: funcIcon.value,
    allowChoice: true,
    showContents: true,
  }

  const methodPointer = input[FunctionInfoKey]?.methodPointer
  if (nameAst.code() !== 'main' && methodPointer != null) {
    widgetInput[FunctionName] = {
      editableNameExpression: nameAst.externalId,
      methodPointer,
    }
  }
  return widgetInput
})
</script>

<template>
  <div class="WidgetFunctionDef">
    <NodeWidget :input="funcNameInput" />
    <ArgumentRow
      v-for="(definition, i) in input.value.argumentDefinitions"
      :key="i"
      :definition="definition"
    />
  </div>
</template>

<script lang="ts">
export const FunctionInfoKey: unique symbol = Symbol.for('WidgetInput:FunctionInfoKey')
declare module '@/providers/widgetRegistry' {
  export interface WidgetInput {
    [FunctionInfoKey]?: {
      methodPointer: MethodPointer
      docsData: Ref<DocumentationData | undefined>
    }
  }
}
export const widgetDefinition = defineWidget(
  WidgetInput.astMatcher(Ast.FunctionDef),
  {
    priority: 999,
    score: Score.Perfect,
  },
  import.meta.hot,
)
</script>

<style scoped>
.WidgetFunctionDef {
  display: flex;
  flex-direction: column;
  align-items: flex-start;
}
</style>
