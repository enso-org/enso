<script setup lang="ts">
import { useVisualizationConfig } from '@/providers/visualizationConfig'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'

const props = defineProps<{}>()

const config = useVisualizationConfig()

function getAstPattern(selector: string | number, action: string) {
  return Pattern.new((ast) =>
    Ast.App.positional(
      Ast.PropertyAccess.new(ast.module, ast, Ast.identifier(action)!),
      typeof selector === 'number' ?
        Ast.tryNumberToEnso(selector, ast.module)!
      : Ast.TextLiteral.new(selector, ast.module),
    ),
  )
}

const createNewNode = () => {
  config.createNodes({
    content: getAstPattern('yes', 'filter'),
    commit: true,
  })
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
