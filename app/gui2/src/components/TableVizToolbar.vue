<script setup lang="ts">
import DropdownMenu from '@/components/DropdownMenu.vue'
import MenuButton from '@/components/MenuButton.vue'
import SvgButton from '@/components/SvgButton.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { useVisualizationConfig } from '@/providers/visualizationConfig'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { computed, ref, watch } from 'vue'
import type { NodeCreationOptions } from './GraphEditor/nodeCreation'
import { TextFormatOptions } from './visualizations/TableVisualization.vue'

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
  isDisabled: boolean
  isFilterSortNodeEnabled: boolean
}>()

const emit = defineEmits<{
  changeFormat: [formatValue: TextFormatOptions]
}>()

const textFormatterSelected = ref(TextFormatOptions.Partial)
watch(textFormatterSelected, (selected) => emit('changeFormat', selected))

const config = useVisualizationConfig()

const sortPatternPattern = computed(() => Pattern.parse('(..Name __ __ )'))

const sortDirection = computed(() => ({
  asc: '..Ascending',
  desc: '..Descending',
}))

const makeSortPattern = (module: Ast.MutableModule) => {
  const columnSortExpressions = props.sortModel
    .filter((sort) => sort?.columnName)
    .sort((a, b) => a.sortIndex - b.sortIndex)
    .map((sort) =>
      sortPatternPattern.value.instantiateCopied([
        Ast.TextLiteral.new(sort.columnName),
        Ast.parse(sortDirection.value[sort.sortDirection as SortDirection]),
      ]),
    )
  return Ast.Vector.new(module, columnSortExpressions)
}

const filterPattern = computed(() => Pattern.parse('__ (__ __)'))

const makeFilterPattern = (module: Ast.MutableModule, columnName: string, items: string[]) => {
  if (
    (items?.length === 1 && items.indexOf('true') != -1) ||
    (items?.length === 1 && items.indexOf('false') != -1)
  ) {
    const boolToInclude = items.indexOf('false') != -1 ? Ast.parse('False') : Ast.parse('True')
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
    Ast.Vector.new(module, itemList),
  ])
}

function getAstPatternSort() {
  return Pattern.new((ast) =>
    Ast.App.positional(
      Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('sort')!),
      makeSortPattern(ast.module),
    ),
  )
}

function getAstPatternFilter(columnName: string, items: string[]) {
  return Pattern.new((ast) =>
    Ast.App.positional(
      Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('filter')!),
      makeFilterPattern(ast.module, columnName, items),
    ),
  )
}

function getAstPatternFilterAndSort(columnName: string, items: string[]) {
  return Pattern.new((ast) =>
    Ast.OprApp.new(
      ast.module,
      Ast.App.positional(
        Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('filter')!),
        makeFilterPattern(ast.module, columnName, items),
      ),
      '.',
      Ast.App.positional(
        Ast.Ident.new(ast.module, Ast.identifier('sort')!),
        makeSortPattern(ast.module),
      ),
    ),
  )
}

const createNewNodes = () => {
  let patterns = new Array<any>()
  if (Object.keys(props.filterModel).length && props.sortModel.length) {
    for (const index in Object.keys(props.filterModel)) {
      const columnName = Object.keys(props.filterModel)[index]!
      const items = props.filterModel[columnName || '']?.values.map((item) => `${item}`)!
      const filterPatterns = getAstPatternFilterAndSort(columnName, items)
      patterns.push(filterPatterns)
    }
  } else if (Object.keys(props.filterModel).length) {
    for (const index in Object.keys(props.filterModel)) {
      const columnName = Object.keys(props.filterModel)[index]!
      const items = props.filterModel[columnName || '']?.values.map((item) => `${item}`)!
      const filterPatterns = getAstPatternFilter(columnName, items)
      patterns.push(filterPatterns)
    }
  } else if (props.sortModel.length) {
    const patSort = getAstPatternSort()
    patterns.push(patSort)
  }
  config.createNodes(
    ...patterns.map(
      (pattern) => ({ content: pattern, commit: true }) satisfies NodeCreationOptions,
    ),
  )
}

const buttonClass = computed(() => {
  return {
    full: isFormatOptionSelected(TextFormatOptions.On),
    partial: isFormatOptionSelected(TextFormatOptions.Partial),
    strikethrough: isFormatOptionSelected(TextFormatOptions.Off),
  }
})

const isFormatOptionSelected = (option: TextFormatOptions): boolean =>
  option === textFormatterSelected.value

const open = ref(false)
const toggleOpen = () => {
  open.value = !open.value
}

const changeFormat = (option: TextFormatOptions) => {
  textFormatterSelected.value = option
  toggleOpen()
}
</script>

<template>
  <div class="TableVizToolbar">
    <DropdownMenu v-model:open="open" class="TextFormattingSelector" title="Text Display Options">
      <template #button
        ><div :class="buttonClass">
          <SvgIcon name="paragraph" /></div
      ></template>

      <template #entries>
        <MenuButton
          class="full"
          title="Text displayed in monospace font and all whitespace characters displayed as symbols"
          @click="() => changeFormat(TextFormatOptions.On)"
        >
          <SvgIcon name="paragraph" />
          <div class="title">Full whitespace rendering</div>
        </MenuButton>

        <MenuButton
          class="partial"
          title="Text displayed in monospace font, only multiple spaces displayed with &#183;"
          @click="() => changeFormat(TextFormatOptions.Partial)"
        >
          <SvgIcon name="paragraph" />
          <div class="title">Partial whitespace rendering</div>
        </MenuButton>

        <MenuButton
          class="off"
          title="No formatting applied to text"
          @click="() => changeFormat(TextFormatOptions.Off)"
        >
          <div class="strikethrough">
            <SvgIcon name="paragraph" />
          </div>
          <div class="title">No whitespace rendering</div>
        </MenuButton>
      </template>
    </DropdownMenu>
  </div>

  <div v-if="isFilterSortNodeEnabled" class="sortFilterNode">
    <SvgButton
      name="add"
      title="Create new component(s) with the current grid's sort and filters applied to the workflow"
      :disabled="props.isDisabled"
      @click="createNewNodes()"
    />
  </div>
</template>

<style scoped>
.TableVizToolbar {
  display: flex;
  flex-direction: row;
  background: var(--color-frame-bg);
  border-radius: 16px;
}

:deep(.DropdownMenuContent) {
  margin-top: 10px;
  padding: 4px;

  > * {
    display: flex;
    padding-left: 8px;
    padding-right: 8px;
  }
}

.strikethrough {
  position: relative;
  margin-right: 4px;
}
.strikethrough:before {
  position: absolute;
  content: '';
  left: 0;
  top: 50%;
  right: 0;
  border-top: 1px solid;
  border-color: black;

  -webkit-transform: rotate(-20deg);
  -moz-transform: rotate(-20deg);
  -ms-transform: rotate(-20deg);
  -o-transform: rotate(-20deg);
  transform: rotate(-20deg);
}

.partial {
  stroke: grey;
  fill: #808080;
}

.off {
  justify-content: flex-start;
}

.full {
  stroke: black;
  fill: #000000;
  justify-content: flex-start;
}

.title {
  padding-left: 2px;
}

.sortFilterNode {
  overflow: hidden;
}
</style>
