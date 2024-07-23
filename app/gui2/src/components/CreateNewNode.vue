<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import { useVisualizationConfig } from '@/providers/visualizationConfig'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'

type SortDirection = 'asc' | 'desc'

const props = defineProps<{
  type: 'sort' | 'filter'
  filterModel?: {
    [key: string]: {
      values: any[]
      filterType: string
    }
  }
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
    const items = props.filterModel[columnName || '']?.values.map((item) => `'${item}'`)
    return `'${columnName}' (..Is_In [${items}] ..Keep)`
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
  config.createNodes({
    content: getAstPatternSort(),
    commit: true,
  })
  config.createNodes({
    content: getAstPatternFilter(),
    commit: true,
  })
}
</script>

<template>
  <SvgButton
    name="add"
    :title="`Create new component with sort and filters applied to the workflow`"
    @click.stop="createNewNode()"
  />
</template>

<style scoped>
.CreateNewNode {
  display: flex;
}
</style>
