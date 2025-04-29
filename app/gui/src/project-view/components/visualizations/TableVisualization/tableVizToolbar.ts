import type { NodeCreationOptions } from '@/components/GraphEditor/nodeCreation'
import type { ToolbarItem } from '@/components/visualizations/toolbar'
import { Ast } from '@/util/ast'
import { Pattern } from '@/util/ast/match'
import type { ToValue } from '@/util/reactivity'
import { computed, type ComputedRef, type Ref, toValue } from 'vue'
import { Expression, MutableExpression } from 'ydoc-shared/ast'
import { TextFormatOptions } from '../TableVisualization.vue'
import {
  actionMap,
  FilterAction,
  FilterType,
  FilterValue,
  FilterValueRange,
  getFilterValue,
  GridFilterModel,
} from './tableVizFilterUtils'

type SortDirection = 'asc' | 'desc'
export type SortModel = {
  columnName: string
  sortDirection: SortDirection
  sortIndex: number
}

export interface SortFilterNodesButtonOptions {
  filterModel: ToValue<GridFilterModel[]>
  sortModel: ToValue<SortModel[]>
  isCreateNewNodeEnabled: ToValue<boolean>
  createNodes: (...options: NodeCreationOptions[]) => void
  getColumnValueToEnso: (
    columnName: string,
  ) => (columnValue: string, module: Ast.MutableModule) => Ast.Owned<Ast.MutableAst>
}

export interface FormatMenuOptions {
  textFormatterSelected: Ref<TextFormatOptions>
}

export interface ColumnNodeButton {
  isCreateNewNodeEnabled: ToValue<boolean>
  createNodes: (...options: NodeCreationOptions[]) => void
  hiddenColumns: ToValue<string[]>
  vizColumnOrder: ToValue<string[] | null>
}

interface NewNodeOptions extends SortFilterNodesButtonOptions, ColumnNodeButton {
  isButtonDisabled: ToValue<boolean>
}

export interface RefreshButtonOptions {
  refreshGrid: () => void
}

export interface Options extends NewNodeOptions, FormatMenuOptions, RefreshButtonOptions {}

/***
 * function that returns a toolbar button item used to apply new nodes to the graph reflecting the sort filter and column changes applied to the current table visualization
 *
 * @param {FilterModel} options.filterModel - The current filter model applied to the table.
 * @param {SortModel} options.sortModel - The current sort model applied to the table.
 * @param {boolean} options.isButtonDisabled - Whether the button should be disabled, the button will be disabled if there are no changes to the table viz.
 * @param {boolean} options.isCreateNewNodeEnabled - Whether the functionality to create new nodes is enabled, only enabled for tables (i.e not rows, vectors).
 * @param options.createNodes - Function to trigger creation of new nodes.
 * @param {(columnId: string, value: unknown) => EnsoValue} options.getColumnValueToEnso - Function to convert column values to a format compatible with Enso.
 * @param {string[]} options.hiddenColumns - A list of column Ids that are currently hidden.
 * @param {string[] | null} options.vizColumnOrder - A list of all column Ids in their new order; if the order is unchanged, this will be null.
 *
 * @returns {ComputedRef<ToolbarItem | undefined>} A computed reference to a toolbar item,
 * or undefined if the button should not be rendered.
 */
function useSortFilterNodesButton({
  filterModel,
  sortModel,
  isButtonDisabled,
  isCreateNewNodeEnabled,
  createNodes,
  getColumnValueToEnso,
  hiddenColumns,
  vizColumnOrder,
}: NewNodeOptions): ComputedRef<ToolbarItem | undefined> {
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

        const value = getFilterValue(filterModel)

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

    function getRemoveColumnsAstPattern() {
      return Pattern.new<Ast.Expression>((ast) => {
        const columns = Ast.Vector.build(columnsToRemove, Ast.TextLiteral.new, ast.module)
        return Ast.App.positional(
          Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('remove_columns')!),
          columns,
        )
      })
    }

    function getColumnOrderAstPattern() {
      return Pattern.new<Ast.Expression>((ast) => {
        const columns = Ast.Vector.build(columnOrder!, Ast.TextLiteral.new, ast.module)
        return Ast.App.positional(
          Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('reorder_columns')!),
          columns,
        )
      })
    }

    const columnsToRemove = toValue(hiddenColumns)
    const columnOrder = toValue(vizColumnOrder)

    if (columnsToRemove.length) {
      patterns.push(getRemoveColumnsAstPattern())
    }
    if (columnOrder != null) {
      patterns.push(getColumnOrderAstPattern())
    }

    createNodes(
      ...patterns.map(
        (pattern) => ({ content: pattern, commit: true }) satisfies NodeCreationOptions,
      ),
    )
  }

  const createNodesButton: ToolbarItem = {
    icon: 'add_to_graph_editor',
    title: "Create new component(s) with the current grid's state applied to the workflow",
    disabled: isButtonDisabled,
    onClick: createNewNodes,
  }

  return computed(() => (toValue(isCreateNewNodeEnabled) ? createNodesButton : undefined))
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

function createRefreshMenu({ refreshGrid }: RefreshButtonOptions): ToolbarItem {
  return {
    title: 'Reset any sort, filter or column changes made to the table',
    icon: 'refresh',
    onClick: refreshGrid,
  }
}

/** TODO: Add docs */
export function useTableVizToolbar(options: Options): ComputedRef<ToolbarItem[]> {
  const createNodesButton = useSortFilterNodesButton(options)
  const formatMenu = createFormatMenu(options)
  const refreshButton = createRefreshMenu(options)
  return computed(() => [
    formatMenu,
    ...(createNodesButton.value ? [createNodesButton.value] : []),
    refreshButton,
  ])
}
