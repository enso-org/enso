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
  console.log(props.sortModel)
  const sortMapping = {
    asc: '..Ascending',
    desc: '..Descending',
  }
  const sorts = []
  for (let [key, value] of props.sortModel) {
    sorts.push(`(..Name '${key}' ${sortMapping[value]})`)
  }
  return sorts
}

function getAstPatternSort() {
  return Pattern.new((ast) =>
    Ast.App.positional(
      Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('sort')!),
      Ast.parse(`[${makeSortPattern()}]`),
    ),
  )
}

function getAstPatternFilter(columnName?: string, items?: string[]) {
  return Pattern.new((ast) =>
    Ast.App.positional(
      Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('filter')!),
      Ast.parse(`'${columnName}' (..Is_In [${items}] ..Keep)`),
    ),
  )
}

const createNewNodes = () => {
  let patterns = new Array<any>()
  if (Object.keys(props.filterModel).length) {
    for (const index in Object.keys(props.filterModel)) {
      const columnName = Object.keys(props.filterModel)[index]
      const items = props.filterModel[columnName || '']?.values.map((item) => `"${item}"`)
      const filterPatterns = getAstPatternFilter(columnName, items)
      patterns.push(filterPatterns)
    }
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
