<script lang="ts">
export const name = 'JSON'
export const icon = 'braces'
export const inputType = 'Standard.Base.Any.Any'
</script>

<script setup lang="ts">
import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import JsonValueWidget from '@/components/visualizations/JSONVisualization/JsonValueWidget.vue'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { stopCopy } from '@/util/shortcuts'
import { useVisualizationConfig } from '@/util/visualizationBuiltins'
import { computed } from 'vue'

const { data } = defineProps<{ data: unknown }>()

const config = useVisualizationConfig()

type ConstructivePattern = (
  placeholder: Ast.Owned<Ast.MutableExpression>,
) => Ast.Owned<Ast.MutableExpression>

const JSON_OBJECT_TYPE = { project: 'Standard.Base', path: 'Data.Json.JS_Object' }

const projectionsEnabled = computed(
  () =>
    config.nodeType?.project === JSON_OBJECT_TYPE.project &&
    config.nodeType.path === JSON_OBJECT_TYPE.path,
)

function projector(parentPattern: ConstructivePattern | undefined) {
  const style = {
    spaced: parentPattern !== undefined,
  }
  return (selector: number | string) => (source: Ast.Owned<Ast.MutableExpression>) =>
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
  if (!projectionsEnabled.value) {
    console.warn('Tried to create Projection in JSON visualization when disabled.')
    return
  }
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
  <div class="JSONVisualization" tabindex="-1" @wheel.stop.passive @keydown="stopCopy">
    <JsonValueWidget
      :data="data"
      indent=""
      :createProjectionCb="projectionsEnabled ? createProjection : null"
    />
  </div>
</template>

<style scoped>
.JSONVisualization {
  padding: 8px;
  user-select: text;
}
</style>
