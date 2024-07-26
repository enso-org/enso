<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import { useVisualizationConfig } from '@/providers/visualizationConfig'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import type { NodeCreationOptions } from './GraphEditor/nodeCreation'

type SortDirection = 'asc' | 'desc'

const props = defineProps<{
  filterModel: {
    [key: string]: {
      values: any[]
      filterType: string
    }
  }
  sortModel: Map<string, SortDirection>
}>()

const config = useVisualizationConfig()

const makeSortPattern = () => {
  const sortMapping = {
    asc: '..Ascending',
    desc: '..Descending',
  }
  const columnName = props.sortModel.keys().next().value
  const direction = props.sortModel.values().next().value as SortDirection
  return `(..Name '${columnName}' ${sortMapping[direction]})`
}

const makeFilterPattern = () => {
  const columnName = Object.keys(props.filterModel)[0]
  const items = props.filterModel[columnName || '']?.values.map((item) => `"${item}"`)
  return `'${columnName}' (..Is_In [${items}] ..Keep)`
}

function getAstPatternSort() {
  return Pattern.new((ast) =>
    Ast.App.positional(
      Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('sort')!),
      Ast.parse(makeSortPattern()),
    ),
  )
}

function getAstPatternFilter() {
  return Pattern.new((ast) =>
    Ast.App.positional(
      Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('filter')!),
      Ast.parse(makeFilterPattern()),
    ),
  )
}

const createNewNodes = () => {
  let patterns = new Array<any>()
  if (Object.keys(props.filterModel).length) {
    const pat = getAstPatternFilter()
    patterns.push(pat)
  }
  if (props.sortModel?.size) {
    const patSort = getAstPatternSort()
    patterns.push(patSort)
  }
  config.createNodes(
    ...patterns.map(
      (pattern) => ({ content: pattern, commit: true }) satisfies NodeCreationOptions,
    ),
  )
}
</script>

<template>
  <SvgButton
    name="add"
    :title="`Create new component(s) with the current grid's sort and filters applied to the workflow`"
    @click="createNewNodes()"
  />
</template>

<style scoped>
.CreateNewNode {
  display: flex;
}
</style>
