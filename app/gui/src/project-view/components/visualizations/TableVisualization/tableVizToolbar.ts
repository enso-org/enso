import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import type { ToolbarItem } from '@/components/visualizations/toolbar'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import type { ToValue } from '@/util/reactivity'
import { computed, type ComputedRef, type Ref, toValue } from 'vue'
import { Expression, MutableExpression } from 'ydoc-shared/ast'
import { TextFormatOptions } from '../TableVisualization.vue'

type SortDirection = 'asc' | 'desc'
export type SortModel = {
  columnName: string
  sortDirection: SortDirection
  sortIndex: number
}
type FilterType = 'number' | 'date' | 'set'

/**
 * Represents the value used for filtering.
 *
 * - For comparisons such as 'equals' or 'greater than,' the filter value is a single value (string).
 * - For 'is in' filtering, the filter value is a list of strings.
 * - For range filtering, the filter value consists of two values that define the range.
 */
type FilterValue = string | string[] | FilterValueRange

const actionMap = {
  equals: '..Equal',
  notEqual: '..Not_Equal',
  greaterThan: '..Greater',
  greaterThanOrEqual: '..Equal_Or_Greater',
  lessThan: '..Less',
  lessThanOrEqual: '..Equal_Or_Less',
  inRange: '..Between',
  blank: '..Is_Nothing',
  notBlank: '..Not_Nothing',
}
type FilterAction = keyof typeof actionMap
export type GridFilterModel = {
  columnName: string
  filterType: FilterType
  filter?: string
  filterTo?: string
  dateFrom?: string
  dateTo?: string
  values?: string[]
  filterAction?: FilterAction
}
type FilterValueRange = {
  toValue: string
  fromValue: string
}

export interface SortFilterNodesButtonOptions {
  filterModel: ToValue<GridFilterModel[]>
  sortModel: ToValue<SortModel[]>
  isDisabled: ToValue<boolean>
  isFilterSortNodeEnabled: ToValue<boolean>
  createNodes: (...options: NodeCreationOptions[]) => void
  getColumnValueToEnso: (
    columnName: string,
  ) => (columnValue: string, module: Ast.MutableModule) => Ast.Owned<Ast.MutableAst>
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
  getColumnValueToEnso,
}: SortFilterNodesButtonOptions): ComputedRef<ToolbarItem | undefined> {
  const sortPatternPattern = computed(() => Pattern.parseExpression('(..Name __ __ )')!)

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
          Ast.parseExpression(sortDirection.value[sort.sortDirection as SortDirection])!,
        ]),
      )
    return Ast.Vector.new(module, columnSortExpressions)
  }

  const filterPattern = computed(() => Pattern.parseExpression('__ (__ __)')!)
  const filterBetweenPattern = computed(() => Pattern.parseExpression('__ (..Between __ __)')!)
  const filterNothingPattern = computed(() => Pattern.parseExpression('__ __')!)

  function makeFilterPattern(module: Ast.MutableModule, columnName: string, items: string[]) {
    if (
      (items?.length === 1 && items.indexOf('true') != -1) ||
      (items?.length === 1 && items.indexOf('false') != -1)
    ) {
      const boolToInclude = Ast.Ident.tryParse(items.indexOf('false') != -1 ? 'False' : 'True')!
      return filterPattern.value.instantiateCopied([
        Ast.TextLiteral.new(columnName),
        Ast.parseExpression('..Equal')!,
        boolToInclude,
      ])
    }
    const valueFormatter = getColumnValueToEnso(columnName)
    if (items?.length === 1) {
      const item = items[0]
      if (item) {
        return filterPattern.value.instantiateCopied([
          Ast.TextLiteral.new(columnName),
          Ast.parseExpression('..Equal')!,
          valueFormatter(item, module) as Expression | MutableExpression,
        ])
      }
    }
    const itemList = items.map((i) => valueFormatter(i, module))
    return filterPattern.value.instantiateCopied([
      Ast.TextLiteral.new(columnName),
      Ast.parseExpression('..Is_In')!,
      Ast.Vector.new(module, itemList),
    ])
  }

  function makeNumericFilterPattern(
    module: Ast.MutableModule,
    columnName: string,
    item: string | FilterValueRange,
    filterAction: FilterAction,
  ) {
    const valueFormatter = getColumnValueToEnso(columnName)
    if (filterAction === 'inRange' && typeof item === 'object') {
      const filterToValue = valueFormatter(item.toValue, module)
      const filterFromValue = valueFormatter(item.fromValue, module)
      return filterBetweenPattern.value.instantiateCopied([
        Ast.TextLiteral.new(columnName),
        filterFromValue as Expression | MutableExpression,
        filterToValue as Expression | MutableExpression,
      ])
    }
    const filterValue = valueFormatter(item as string, module)
    const action = actionMap[filterAction]
    return filterPattern.value.instantiateCopied([
      Ast.TextLiteral.new(columnName),
      Ast.parseExpression(action)!,
      filterValue as Expression | MutableExpression,
    ])
  }

  function makeNothingFilterPattern(columnName: string, filterAction: FilterAction) {
    const action = actionMap[filterAction]
    return filterNothingPattern.value.instantiateCopied([
      Ast.TextLiteral.new(columnName),
      Ast.parseExpression(action)!,
    ])
  }

  function getAstPatternSort() {
    return Pattern.new<Ast.Expression>((ast) =>
      Ast.App.positional(
        Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('sort')!),
        makeSortPattern(ast.module),
      ),
    )
  }

  function getAstPatternFilter(
    columnName: string,
    items: string[] | string | FilterValue,
    filterType: FilterType,
    filterAction?: FilterAction,
  ) {
    return Pattern.new<Ast.Expression>((ast) =>
      Ast.App.positional(
        Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('filter')!),
        filterType === 'set' ?
          makeFilterPattern(ast.module, columnName, items as string[])
        : makeNumericFilterPattern(
            ast.module,
            columnName,
            items as string | FilterValueRange,
            filterAction!,
          ),
      ),
    )
  }

  function getAstNothingPatternFilter(columnName: string, filterAction: FilterAction) {
    return Pattern.new<Ast.Expression>((ast) =>
      Ast.App.positional(
        Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('filter')!),
        makeNothingFilterPattern(columnName, filterAction),
      ),
    )
  }

  function getAstPatternFilterAndSort(
    columnName: string,
    items: string[] | string | FilterValueRange,
    filterType: FilterType,
    filterAction?: FilterAction,
  ) {
    return Pattern.new<Ast.Expression>((ast) =>
      Ast.OprApp.new(
        ast.module,
        Ast.App.positional(
          Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('filter')!),
          filterType === 'set' ?
            makeFilterPattern(ast.module, columnName, items as string[])
          : makeNumericFilterPattern(
              ast.module,
              columnName,
              items as string | FilterValueRange,
              filterAction!,
            ),
        ),
        '.',
        Ast.App.positional(
          Ast.Ident.new(ast.module, Ast.identifier('sort')!),
          makeSortPattern(ast.module),
        ),
      ),
    )
  }

  function getAstNothingPatternFilterAndSort(columnName: string, filterAction: FilterAction) {
    return Pattern.new<Ast.Expression>((ast) =>
      Ast.OprApp.new(
        ast.module,
        Ast.App.positional(
          Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('filter')!),
          makeNothingFilterPattern(columnName, filterAction),
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
    if (filterModelValue.length) {
      filterModelValue.map((filterModel: GridFilterModel) => {
        const columnName = filterModel.columnName
        const filterAction = filterModel.filterAction
        const filterType = filterModel.filterType
        if (filterAction === 'blank' || filterAction === 'notBlank') {
          const filterPatterns =
            sortModelValue.length ?
              getAstNothingPatternFilterAndSort(columnName, filterAction)
            : getAstNothingPatternFilter(columnName, filterAction)
          patterns.push(filterPatterns)
        }

        let value: FilterValue
        switch (filterType) {
          case 'number':
            value =
              filterAction === 'inRange' ?
                { toValue: filterModel.filterTo!, fromValue: filterModel.filter! }
              : (filterModel.filter as FilterValue)
            break
          case 'date':
            value =
              filterAction === 'inRange' ?
                { toValue: filterModel.dateTo!, fromValue: filterModel.dateFrom! }
              : (filterModel.dateFrom as FilterValue)
            break
          default:
            value = filterModel.values as FilterValue
        }
        if (value) {
          const filterPatterns =
            sortModelValue.length ?
              getAstPatternFilterAndSort(columnName, value, filterType, filterAction)
            : getAstPatternFilter(columnName, value, filterType, filterAction)
          patterns.push(filterPatterns)
        }
      })
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
          stroke: '#525252',
          color: '#525252',
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
