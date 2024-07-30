<script setup lang="ts">
import SvgButton from '@/components/SvgButton.vue'
import { useVisualizationConfig } from '@/providers/visualizationConfig'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { computed } from 'vue'
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

const sortPatternPattern = computed(() => Pattern.parse('(..Name __ __ )'))

const sortDirection = computed(() => ({
  asc: '..Ascending',
  desc: '..Descending',
}))

const makeSortPattern = (ast: Ast.Owned<Ast.MutableAst>) => {
  const columnSortExpressions = props.sortModel
    .filter((sort) => sort?.columnName)
    .sort((a, b) => a.sortIndex - b.sortIndex)
    .map((sort) =>
      sortPatternPattern.value.instantiateCopied([
        Ast.TextLiteral.new(sort.columnName),
        Ast.parse(sortDirection.value[sort.sortDirection as SortDirection]),
      ]),
    )
  return Ast.Vector.new(ast.module, columnSortExpressions)
}

const filterPattern = computed(() => Pattern.parse('__ (__ __)'))

const makeFilterPattern = (ast: Ast.Owned<Ast.MutableAst>, columnName: string, items: string[]) => {
  if (
    (items?.length === 1 && items.indexOf('true') != -1) ||
    (items?.length === 1 && items.indexOf('false') != -1)
  ) {
    const boolToInclude = items.indexOf('"false"') != -1 ? Ast.parse('False') : Ast.parse('True')
    return filterPattern.value.instantiateCopied([
      Ast.TextLiteral.new(columnName),
      Ast.parse('..Equal'),
      boolToInclude,
    ])
  }
  const itemList = items.map((i) => Ast.TextLiteral.new(i))
  return filterPattern.value.instantiateCopied([
    Ast.TextLiteral.new(columnName),
    Ast.parse('..Is_In'),
    Ast.Vector.new(ast.module, itemList),
  ])
}

function getAstPatternSort() {
  return Pattern.new((ast) =>
    Ast.App.positional(
      Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('sort')!),
      makeSortPattern(ast),
    ),
  )
}

function getAstPatternFilter(columnName: string, items: string[]) {
  return Pattern.new((ast) =>
    Ast.App.positional(
      Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('filter')!),
      makeFilterPattern(ast, columnName, items),
    ),
  )
}

const createNewNodes = () => {
  let patterns = new Array<any>()
  if (Object.keys(props.filterModel).length) {
    for (const index in Object.keys(props.filterModel)) {
      const columnName = Object.keys(props.filterModel)[index]!
      const items = props.filterModel[columnName || '']?.values.map((item) => `${item}`)!
      const filterPatterns = getAstPatternFilter(columnName, items)
      patterns.push(filterPatterns)
    }
  }
  if (props.sortModel.length) {
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
