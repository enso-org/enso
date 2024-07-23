<script setup lang="ts">
import { useVisualizationConfig } from '@/providers/visualizationConfig'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'

type SortDirection = 'asc' | 'desc'

const props = defineProps<{
  type: 'sort' | 'filter'
  filterModel?: { [key: string]: string }
  sortModel?: Map<string, SortDirection>
}>()

const config = useVisualizationConfig()

const makeSortPattern = () => {
  const sortMapping = {
    asc: '..Ascending',
    desc: '..Descending',
  }
  if (props.sortModel?.size) {
    const columnName = props.sortModel.keys().next().value
    const direction = props.sortModel.values().next().value as SortDirection
    return `(..Name '${columnName}' ${sortMapping[direction]})`
  }
}

const makeFilterPattern = () => {
  if (props.filterModel) {
    const columnName = Object.keys(props.filterModel)[0]
    const items = props.filterModel[columnName || '']
    console.log({ items })
    return `'${columnName}' (..Equal '${items}' ..Remove)`
  }
}

function getAstPatternSort() {
  return Pattern.new((ast) =>
    Ast.App.positional(
      Ast.PropertyAccess.new(ast.module, ast, Ast.identifier(props.type)!),
      Ast.parse(makeSortPattern() || ''),
    ),
  )
}

function getAstPatternFilter() {
  return Pattern.new((ast) =>
    Ast.App.positional(
      Ast.PropertyAccess.new(ast.module, ast, Ast.identifier(props.type)!),
      Ast.parse(makeFilterPattern() || ''),
    ),
  )
}

const createNewNode = () => {
  const { filterModel, sortModel } = props
  console.log({ filterModel })
  console.log({ sortModel })
  config.createNodes({
    content: props.type === 'sort' ? getAstPatternSort() : getAstPatternFilter(),
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
