import { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import { TextFormatOptions } from '@/components/visualizations/TableVisualization.vue'
import { ToolbarItem } from '@/components/visualizations/toolbar'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import { ToValue } from '@/util/reactivity'
import { computed, ComputedRef, Ref, toValue } from 'vue'

type SortDirection = 'asc' | 'desc'
export type SortModel = {
  columnName: string
  sortDirection: SortDirection
  sortIndex: number
}

export interface SortFilterNodesButtonOptions {
  filterModel: ToValue<{
    [key: string]: {
      values: any[]
      filterType: string
    }
  }>
  sortModel: ToValue<SortModel[]>
  isDisabled: ToValue<boolean>
  isFilterSortNodeEnabled: ToValue<boolean>
  createNodes: (...options: NodeCreationOptions[]) => void
}

export interface FormatMenuOptions {
  textFormatterSelected: Ref<TextFormatOptions>
}

export interface Options extends SortFilterNodesButtonOptions, FormatMenuOptions {}

function useSortFilterNodesButton({
  filterModel,
  sortModel,
  isDisabled,
  isFilterSortNodeEnabled,
  createNodes,
}: SortFilterNodesButtonOptions): ComputedRef<ToolbarItem | undefined> {
  const sortPatternPattern = computed(() => Pattern.parse('(..Name __ __ )'))

  const sortDirection = computed(() => ({
    asc: '..Ascending',
    desc: '..Descending',
  }))

  function makeSortPattern(module: Ast.MutableModule) {
    const columnSortExpressions = toValue(sortModel)
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

  function makeFilterPattern(module: Ast.MutableModule, columnName: string, items: string[]) {
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

  function createNewNodes() {
    const patterns = new Array<Pattern>()
    const filterModelValue = toValue(filterModel)
    const sortModelValue = toValue(sortModel)
    if (Object.keys(filterModelValue).length) {
      for (const [columnName, columnFilter] of Object.entries(filterModelValue)) {
        const items = columnFilter.values.map((item) => `${item}`)
        const filterPatterns =
          sortModelValue.length ?
            getAstPatternFilterAndSort(columnName, items)
          : getAstPatternFilter(columnName, items)
        patterns.push(filterPatterns)
      }
    } else if (sortModelValue.length) {
      patterns.push(getAstPatternSort())
    }
    createNodes(
      ...patterns.map(
        (pattern) => ({ content: pattern, commit: true }) satisfies NodeCreationOptions,
      ),
    )
  }

  const createNodesButton: ToolbarItem = {
    icon: 'add_to_graph_editor',
    title:
      "Create new component(s) with the current grid's sort and filters applied to the workflow",
    disabled: isDisabled,
    onClick: createNewNodes,
  }

  return computed(() => (toValue(isFilterSortNodeEnabled) ? createNodesButton : undefined))
}

function createFormatMenu({ textFormatterSelected }: FormatMenuOptions): ToolbarItem {
  return {
    selected: textFormatterSelected,
    title: 'Text Display Options',
    options: {
      full: {
        icon: 'paragraph',
        iconStyle: {
          stroke: 'black',
          color: 'black',
        },
        title:
          'Text displayed in monospace font and all whitespace characters displayed as symbols',
        label: 'Full whitespace rendering',
      },
      partial: {
        icon: 'paragraph',
        iconStyle: {
          stroke: 'grey',
          color: 'grey',
        },
        title: 'Text displayed in monospace font, only multiple spaces displayed with "\xB7"',
        label: 'Partial whitespace rendering',
      },
      off: {
        icon: 'not_paragraph',
        title: 'No formatting applied to text',
        label: 'No whitespace rendering',
      },
    },
  }
}

/** TODO: Add docs */
export function useTableVizToolbar(options: Options): ComputedRef<ToolbarItem[]> {
  const createNodesButton = useSortFilterNodesButton(options)
  const formatMenu = createFormatMenu(options)
  return computed(() => [formatMenu, ...(createNodesButton.value ? [createNodesButton.value] : [])])
}
