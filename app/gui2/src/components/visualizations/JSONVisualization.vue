<script lang="ts">
export const name = 'JSON'
export const icon = 'braces'
export const inputType = 'Any'
</script>

<script setup lang="ts">
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import JsonValueWidget from '@/components/visualizations/JSONVisualization/JsonValueWidget.vue'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { useVisualizationConfig, VisualizationContainer } from '@/util/visualizationBuiltins'

const props = defineProps<{ data: unknown }>()

const config = useVisualizationConfig()

type ConstructivePattern = (placeholder: Ast.Owned) => Ast.Owned

function projector(parentPattern: ConstructivePattern | undefined) {
  const style = {
    spaced: parentPattern !== undefined,
  }
  return (selector: number | string) => (source: Ast.Owned) =>
    Ast.App.positional(
      Ast.PropertyAccess.new(
        source.module,
        parentPattern ? parentPattern(source) : source,
        Ast.identifier('get')!,
        style,
      ),
      typeof selector === 'number' ?
        Ast.tryNumberToEnso(selector, source.module)!
      : Ast.TextLiteral.new(selector, source.module),
      source.module,
    )
}

function createProjection(path: (string | number)[][]) {
  let patterns = new Array<ConstructivePattern>()
  for (const level of path)
    patterns = (patterns.length ? patterns : [undefined]).flatMap((parent) =>
      level.map(projector(parent)),
    )
  config.createNodes(
    ...patterns.map(
      (pattern) => ({ content: Pattern.new(pattern), commit: true }) satisfies NodeCreationOptions,
    ),
  )
}
</script>

<template>
  <VisualizationContainer :belowToolbar="true">
    <div class="JSONVisualization">
      <JsonValueWidget :data="props.data" @createProjection="createProjection" />
    </div>
  </VisualizationContainer>
</template>

<style scoped>
.JSONVisualization {
  font-family: var(--font-mono);
  padding: 8px;
}
</style>
