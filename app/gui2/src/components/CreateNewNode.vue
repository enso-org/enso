<script setup lang="ts">
import { useVisualizationConfig } from '@/providers/visualizationConfig'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'

const props = defineProps<{
  type: string
  filterModel?: { [key: string]: string }
  sortModel?: Map<string, string>
}>()

const config = useVisualizationConfig()

function getAstPattern() {
  const colName = 'Sales Date'
  return Pattern.new((ast) =>
    Ast.App.positional(
      Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('sort')!),
      Ast.parse(`[(..Name '${colName}' ..Ascending)]`),
    ),
  )
}

// function getAstPattern(action: string, columnName: string | undefined)
// Pattern.new((ast) =>
//   Ast.OprApp.new(
//     ast.module,
//     Ast.App.positional(
//       Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('rows')!),
//       Ast.parse('(..All_Rows)'),
//     ),
//     '.',
//     Ast.App.positional(
//       Ast.Ident.new(ast.module, Ast.identifier('get')!),
//       Ast.tryNumberToEnso(index, ast.module)!,
//     ),
//   ),
// )

const createNewNode = () => {
  const { filterModel, sortModel } = props
  console.log({ filterModel })
  console.log({ sortModel })
  config.createNodes({
    content: getAstPattern(),
    commit: true,
  })
}
</script>

<template>
  <div class="CreateNewNode">
    <button :onclick="createNewNode">CREATE COMPONENT {{ props.type }}</button>
  </div>
</template>

<style scoped>
.CreateNewNode {
  display: flex;
}
</style>
