import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import type { ToolbarItem } from '@/components/visualizations/toolbar'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import type { ToValue } from '@/util/reactivity'
import { computed, type ComputedRef, type Ref, toValue } from 'vue'
import { Expression, MutableExpression } from 'ydoc-shared/ast'
import { TextFormatOptions } from '../TableVisualization.vue'
import { filter } from 'd3'

type SortDirection = 'asc' | 'desc'
export type SortModel = {
  columnName: string
  sortDirection: SortDirection
  sortIndex: number
}
type FilterType = 'number' | 'date' | 'set'
type FilterAction = 'equals' | 'notEqual' | 'greaterThan' | "greaterThanOrEqual" | 'lessThan' | 'lessThanOrEqual' | 'inRange' | 'blank' | 'notBlank'
export type FilterModel = {
  columnName: string
  filterType: FilterType;
  filter?: string; //value filtering on in a numeric filter
  filterTo?: string; //needed for numeric between filters 
  dateFrom?: string;
  dateTo?: string;
  values?: string[]; //used for set filter
  filterAction?: FilterAction;
}

const actionMap = {
  'equals': '..Equals', 
  'notEqual' : '..Not_Equal',
  'greaterThan' : '..Greater',
  'greaterThanOrEqual' : '..Equal_Or_Greater',
  'lessThan' : '..Less',
  'lessThanOrEqual' : '..Equal_Or_Less',
  'inRange' : '..Between',
}

export interface SortFilterNodesButtonOptions {
  filterModel: ToValue<FilterModel[]>
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

  function makeNumericFilterPattern(module: Ast.MutableModule, columnName: string, item: string, filterAction: FilterAction) {
    const valueFormatter = getColumnValueToEnso(columnName)
    const filterValue = valueFormatter(item, module)
    const action = actionMap[filterAction]
    return filterPattern.value.instantiateCopied([
      Ast.TextLiteral.new(columnName),
      Ast.parseExpression(action)!,
      filterValue,
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

  function getAstPatternFilter(columnName: string, items: string[] | string, filterType: FilterType, filterAction?: FilterAction) {
    return Pattern.new<Ast.Expression>((ast) =>
      Ast.App.positional(
        Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('filter')!),
        filterType === 'set' ? makeFilterPattern(ast.module, columnName, items as string[]) : makeNumericFilterPattern(ast.module, columnName, items as string, filterAction!),
      ),
    )
  }

  function getAstPatternFilterAndSort(columnName: string, items: string[] | string, filterType: FilterType, filterAction?: FilterAction) {
    return Pattern.new<Ast.Expression>((ast) =>
      Ast.OprApp.new(
        ast.module,
        Ast.App.positional(
          Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('filter')!),
          filterType === 'set' ? makeFilterPattern(ast.module, columnName, items as string[]) : makeNumericFilterPattern(ast.module, columnName, items as string, filterAction!),
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
    console.log({filterModelValue})
    if (filterModelValue.length) {
      console.log('HELLO')
      filterModelValue.map((filterModel: FilterModel) => {
        const columnName = filterModel.columnName
        if(filterModel.filterAction === 'blank' || filterModel.filterAction === 'notBlank') {
          //BLANK/NOT BLANK FILTER
        }
        if(filterModel.filterAction === 'inRange') {
          //BETWEEN FILTER
        }
        if(filterModel.filterType === 'number') {
          const value = filterModel.filter
          if(value) {
            const filterPatterns =
            sortModelValue.length ?
              getAstPatternFilterAndSort(columnName, value, filterModel.filterType, filterModel.filterAction)
            : getAstPatternFilter(columnName, value, filterModel.filterType, filterModel.filterAction)
          patterns.push(filterPatterns)
          }
        }
        if(filterModel.filterType === 'date') {
          const date = filterModel.dateFrom
          if(date) {
            const filterPatterns =
            sortModelValue.length ?
              getAstPatternFilterAndSort(columnName, date, filterModel.filterType, filterModel.filterAction)
            : getAstPatternFilter(columnName, date, filterModel.filterType, filterModel.filterAction)
          patterns.push(filterPatterns)
          }
        } 
        if(filterModel.filterType === 'set') {
          console.log('HELLO2')
          console.log({filterModel})
          const items = filterModel.values
          console.log({items})
          if(items) {
            const filterPatterns =
            sortModelValue.length ?
              getAstPatternFilterAndSort(columnName, items, filterModel.filterType, filterModel.filterAction)
            : getAstPatternFilter(columnName, items, filterModel.filterType, filterModel.filterAction)
          patterns.push(filterPatterns)
          }
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
