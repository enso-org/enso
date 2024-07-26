<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import { useVisualizationConfig } from '@/providers/visualizationConfig'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import type { NodeCreationOptions } from './GraphEditor/nodeCreation'

type SortDirection = 'asc' | 'desc'
export type SortModel = {
  columnName: string
  sortDirection: SortDirection
  sortIndex: number
}

const props = defineProps<{
  filterModel: {
    [key: string]: {
      values: any[]
      filterType: string
    }
  }
  sortModel: SortModel[]
}>()

const config = useVisualizationConfig()

const makeSortPattern = () => {
  const sortMapping = {
    asc: '..Ascending',
    desc: '..Descending',
  }
  return props.sortModel
    .filter((sort) => sort?.columnName)
    .sort((a, b) => a.sortIndex - b.sortIndex)
    .map((sort) => {
      return `(..Name '${sort.columnName}' ${sortMapping[sort.sortDirection]})`
    })
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
  if (
    (items?.length === 1 && items.indexOf('"true"') != -1) ||
    (items?.length === 1 && items.indexOf('"false"') != -1)
  ) {
    const boolToInclude = items.indexOf('"false"') != -1 ? 'False' : 'True'
    return Pattern.new((ast) =>
      Ast.App.positional(
        Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('filter')!),
        Ast.parse(`'${columnName}' (..Equal ${boolToInclude})`),
      ),
    )
  }
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
  if (Object.keys(props.sortModel).length) {
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
