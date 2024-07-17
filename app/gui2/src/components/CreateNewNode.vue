<script setup lang="ts">
import { useVisualizationConfig } from '@/providers/visualizationConfig'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'

const props = defineProps<{
  filterModel: { [key: string]: string }
  sortModel: Map<string, string>
}>()

const config = useVisualizationConfig()

function getAstPattern(action: string, columnName: string | undefined) {
  return Pattern.new((ast) =>
    Ast.App.positional(
      Ast.PropertyAccess.new(ast.module, ast, Ast.identifier(action)!),
      Ast.TextLiteral.new(columnName || '', ast.module),
    ),
  )
}

const createNewNode = () => {
  if (props.sortModel.size) {
    config.createNodes({
      content: getAstPattern('sort', props.sortModel.keys().next().value),
      commit: true,
    })
  }
}
</script>

<template>
  <div class="CreateNewNode">
    <button :onclick="createNewNode">CREATE COMPONENT</button>
  </div>
</template>

<style scoped>
.CreateNewNode {
  display: flex;
}
</style>
